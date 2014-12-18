(*
 * Copyright (C) Citrix Systems Inc.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Lwt
open Sexplib.Std
open Xenstore

(** A byte-level transport over the xenstore Unix domain socket *)

include IO_lwt

let debug fmt = Logging.debug "userspace" fmt
let error fmt = Logging.error "userspace" fmt

(* If we get a connection refused error it will be because the server
 * is still starting up. *)
let initial_retry_interval = 0.1 (* seconds *)
let max_retry_interval = 5.0 (* seconds *)
let retry_max = 100 (* attempts *)

let max_packet_size = Protocol.xenstore_payload_max + Protocol.Header.sizeof

exception Connection_timeout

module FDReader = struct
  type stream = {
    fd: Lwt_unix.file_descr;
    buffer: Cstruct.t;
    mutable offset: int64;
    mutable length: int;
  }
  type position = int64 with sexp
  type item = Cstruct.t

  include IO_lwt

  let make fd =
    let buffer = Cstruct.create max_packet_size in
    let offset = 0L in
    let length = 0 in
    { fd; buffer; offset; length }

  let read stream =
    match stream.length with
    | 0 ->
      Lwt_cstruct.read stream.fd (Cstruct.shift stream.buffer stream.length) >>= fun n ->
      stream.length <- stream.length + n;
      return (stream.offset, `Ok (Cstruct.sub stream.buffer 0 stream.length))
    | n ->
      return (stream.offset, `Ok (Cstruct.sub stream.buffer 0 stream.length))

  let advance stream offset =
    let delta = Int64.(to_int (sub offset stream.offset)) in
    let length = max 0 (stream.length - delta) in
    if length > 0
    then Cstruct.blit stream.buffer (stream.length - length) stream.buffer 0 length;
    stream.offset <- offset;
    stream.length <- length;
    return ()
end

let complete op fd buf =
  let ofs = buf.Cstruct.off in
  let len = buf.Cstruct.len in
  let buf = buf.Cstruct.buffer in
  let rec loop acc fd buf ofs len =
    op fd buf ofs len >>= fun n ->
    let len' = len - n in
    let acc' = acc + n in
    if len' = 0 || n = 0
    then return acc'
    else loop acc' fd buf (ofs + n) len' in
  loop 0 fd buf ofs len >>= fun n ->
  if n = 0 && len <> 0
  then fail End_of_file
  else return ()

module FDWriter = struct
  type stream = {
    fd: Lwt_unix.file_descr;
    buffer: Cstruct.t;
    mutable offset: int64;
  }
  type position = int64 with sexp
  type item = Cstruct.t

  include IO_lwt

  let make fd =
    let buffer = Cstruct.create max_packet_size in
    let offset = 0L in
    { fd; buffer; offset }

  let read stream =
    return (stream.offset, `Ok stream.buffer )

  let advance stream offset =
    let delta = Int64.(to_int (sub offset stream.offset)) in
    complete Lwt_bytes.write stream.fd (Cstruct.sub stream.buffer 0 delta) >>= fun () ->
    let unwritten = Cstruct.len stream.buffer - delta in
    if unwritten > 0
    then Cstruct.blit stream.buffer (Cstruct.len stream.buffer - unwritten) stream.buffer 0 unwritten;
    stream.offset <- offset;
    return ()
end

(* Individual connections *)
type connection = {
  fd: Lwt_unix.file_descr;
  sockaddr: Lwt_unix.sockaddr;
  reader: FDReader.stream;
  writeBuffers: FDWriter.stream;
}


module Request = struct
  type item = Protocol.Header.t * Protocol.Request.t

  module PacketReader = PacketReader.Make(Protocol.Request)(FDReader)
  module PacketWriter = PacketWriter.Make(Protocol.Request)(FDWriter)

  module Reader = struct
    include IO_lwt
    type stream = connection
    type position = int64 with sexp
    type item = Protocol.Header.t * Protocol.Request.t
    let read stream = PacketReader.read stream.reader
    let advance stream = PacketReader.advance stream.reader
  end
  module Writer = struct
    include IO_lwt
    type stream = connection
    type position = int64 with sexp
    type item = Protocol.Header.t * Protocol.Request.t
    let write stream = PacketWriter.write stream.writeBuffers
    let advance stream = PacketWriter.advance stream.writeBuffers
  end
end

module Response = struct
  type item = Protocol.Header.t * Protocol.Response.t

  module PacketReader = PacketReader.Make(Protocol.Response)(FDReader)
  module PacketWriter = PacketWriter.Make(Protocol.Response)(FDWriter)

  module Reader = struct
    include IO_lwt
    type stream = connection
    type position = int64 with sexp
    type item = Protocol.Header.t * Protocol.Response.t
    let read stream = PacketReader.read stream.reader
    let advance stream = PacketReader.advance stream.reader
  end
  module Writer = struct
    include IO_lwt
    type stream = connection
    type position = int64 with sexp
    type item = Protocol.Header.t * Protocol.Response.t
    let write stream = PacketWriter.write stream.writeBuffers
    let advance stream = PacketWriter.advance stream.writeBuffers
  end
end

let alloc (fd, sockaddr) =
  let reader = FDReader.make fd in
  let writeBuffers = FDWriter.make fd in
  return { fd; sockaddr; reader; writeBuffers }

let xenstored_socket = ref "/var/run/xenstored/socket"

(* We'll look for these paths in order: *)
let get_xenstore_paths () =
  let default = [
    !xenstored_socket;
    "/proc/xen/xenbus"; (* Linux *)
    "/dev/xen/xenstore"; (* FreeBSD *)
  ] in
  try
    Sys.getenv "XENSTORED_PATH" :: default
  with Not_found -> default

let choose_xenstore_path () =
  List.fold_left (fun acc possibility -> match acc with
      | Some x -> Some x
      | None ->
        if Sys.file_exists possibility then Some possibility else None
    ) None (get_xenstore_paths ())

exception Could_not_find_xenstore

let create () =
  ( match choose_xenstore_path () with
    | None ->
      error "Failed to find xenstore socket. I tried the following:";
      List.iter (fun x -> error "  %s" x) (get_xenstore_paths ());
      error "On linux you might not have xenfs mounted:";
      error "   sudo mount -t xenfs xenfs /proc/xen";
      error "Or perhaps you just need to set the XENSTORED_PATH environment variable.";
      fail Could_not_find_xenstore
    | Some x -> return x ) >>= fun path ->
  Lwt_unix.stat path >>= fun stats ->
  let sockaddr = Lwt_unix.ADDR_UNIX(path) in
  match stats.Lwt_unix.st_kind with
  | Lwt_unix.S_SOCK ->
    let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
    let start = Unix.gettimeofday () in
    let rec retry n interval =
      if n > retry_max then begin
        error "Failed to connect after %.0f seconds" (Unix.gettimeofday () -. start);
        fail Connection_timeout
      end else
        try_lwt
          Lwt_unix.connect fd sockaddr
        with Unix.Unix_error(Unix.ECONNREFUSED, _, _) ->
          Lwt_unix.sleep interval >>= fun () ->
          retry (n + 1) (interval +. 0.1) in
    retry 0 initial_retry_interval >>= fun () ->
    alloc (fd, sockaddr)
  | _ ->
    let fd = Unix.openfile path [ Lwt_unix.O_RDWR ] 0o0 in
    (* It looks like a file but behaves like a pipe: *)
    alloc (Lwt_unix.of_unix_file_descr ~blocking:false fd, sockaddr)

let destroy { fd } = Lwt_unix.close fd

let int_of_file_descr fd =
  let fd = Lwt_unix.unix_file_descr fd in
  let (fd: int) = Obj.magic fd in
  fd

let uri_of { fd } =
  let creds = Lwt_unix.get_credentials fd in
  let pid = creds.Lwt_unix.cred_pid in
  lwt cmdline =
    Lwt_io.with_file ~mode:Lwt_io.input
      (Printf.sprintf "/proc/%d/cmdline" pid)
      (fun ic ->
         lwt cmdline = Lwt_io.read_line_opt ic in
         match cmdline with
         | Some x -> return x
         | None -> return "unknown") in
  (* Take only the binary name, stripped of directories *)
  let filename =
    try
      let i = String.index cmdline '\000' in
      String.sub cmdline 0 i
    with Not_found -> cmdline in
  let basename = Filename.basename filename in
  let name = Printf.sprintf "%d:%s:%d" pid basename (int_of_file_descr fd) in
  return (Uri.make ~scheme:"unix" ~path:name ())

let domain_of _ = 0

(* Servers which accept connections *)
type server = Lwt_unix.file_descr

let _ =
  (* Make sure a write to a closed fd doesn't cause us to quit
     	   with SIGPIPE *)
  Sys.set_signal Sys.sigpipe Sys.Signal_ignore

let listen () =
  let sockaddr = Lwt_unix.ADDR_UNIX(!xenstored_socket) in
  let fd = Lwt_unix.socket Lwt_unix.PF_UNIX Lwt_unix.SOCK_STREAM 0 in
  lwt () = try_lwt Lwt_unix.unlink !xenstored_socket with _ -> return () in
  Lwt_unix.bind fd sockaddr;
  Lwt_unix.listen fd 5;
  return fd

let rec accept_forever fd process =
  lwt conns, _ (*exn_option*) = Lwt_unix.accept_n fd 16 in
  let (_: unit Lwt.t list) = List.map (fun x -> alloc x >>= process) conns in
  accept_forever fd process

module Introspect = struct
  type t = connection

  let read { fd } = function
    | [ "readable" ] -> Some (string_of_bool (Lwt_unix.readable fd))
    | [ "writable" ] -> Some (string_of_bool (Lwt_unix.writable fd))
    | _ -> None

  let ls t = function
    | [] -> [ "readable"; "writable" ]
    | _ -> []

  let write _ _ _ = false
end

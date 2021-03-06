(*
 * Copyright (c) 2010-2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012-14 Citrix Systems Inc
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
open Printf
open Conproto
open Gnt

module type ACTIVATIONS = sig

  (** Event channels handlers. *)

  type event
  (** identifies the an event notification received from xen *)

  val program_start: event
  (** represents an event which 'fired' when the program started *)

  val after: Eventchn.t -> event -> event Lwt.t
  (** [next channel event] blocks until the system receives an event
      newer than [event] on channel [channel]. If an event is received
      while we aren't looking then this will be remembered and the
      next call to [after] will immediately unblock. If the system
      is suspended and then resumed, all event channel bindings are invalidated
      and this function will fail with Generation.Invalid *)
end


type stats = {
  mutable total_read: int; (* bytes read *)
  mutable total_write: int; (* bytes written *)
}

type t = {
  domid:  int;
  xg:     Gnttab.interface;
  xe:     Eventchn.handle;
  evtchn: Eventchn.t;
  ring:   Cstruct.t;
}

module ConsoleError = struct
  open Lwt
  let (>>=) x f = x >>= function
  | `Ok x -> f x
  | `Error (`Invalid_console x) -> fail (Failure (Printf.sprintf "Invalid_console %s" x))
  | `Error _ -> fail (Failure "unknown console device failure")
end

module type CONSOLE = sig
   include V1_LWT.FLOW
     with type 'a io = 'a Lwt.t
end

module Make(A: ACTIVATIONS)(X: Xenstore.S.CLIENT)(C: CONSOLE) = struct

  let service_thread t c stats unbind_fn =

    let (>>|=) m f =
      let open Lwt in
      m >>= function
      | `Ok x -> f x
      | `Eof ->
        fail (Failure "End of file")
      | `Error _ ->
        fail (Failure "Failure reading from FLOW") in

    let shutdown_in_progress = ref false in
    let shutdown_c = Lwt_condition.create () in
    let request_shutdown () =
      shutdown_in_progress := true;
      Lwt_condition.broadcast shutdown_c () in
    let rec wait_shutdown () =
      let open Lwt in
      if !shutdown_in_progress
      then return `Shutdown
      else Lwt_condition.wait shutdown_c >>= fun () -> wait_shutdown () in

    let rec read_the_ring after : unit Lwt.t =
      let open Lwt in
      let seq, avail = Console_ring.Ring.Back.Reader.read t.ring in
      let n = Cstruct.len avail in
      if n > 0 then begin
        C.write c avail >>|= fun () ->
        stats.total_read <- stats.total_read + n;
        let seq = Int32.(add seq (of_int n)) in
        Console_ring.Ring.Back.Reader.advance t.ring seq;
        Eventchn.notify t.xe t.evtchn;
        read_the_ring after
      end else
        pick [ map (fun e -> `Next e) (A.after t.evtchn after);
               wait_shutdown () ]
        >>= function
        | `Next next ->
          read_the_ring next
        | `Shutdown ->
          return () in

    let rec read_the_flow after : unit Lwt.t =
      let open Lwt in
      pick [ map (fun x -> `Flow x) (C.read c);
             wait_shutdown () ]
      >>= function
      | `Shutdown -> return ()
      | `Flow x ->
      (return x) >>|= fun buffer ->
      let rec loop after buffer =
        if Cstruct.len buffer = 0
        then return (`Next after)
        else begin
          let seq, avail = Console_ring.Ring.Back.Writer.write t.ring in
          if Cstruct.len avail = 0 then begin
            pick [ map (fun e -> `Next e) (A.after t.evtchn after);
                   wait_shutdown () ]
            >>= function
            | `Next next -> loop next buffer
            | `Shutdown -> return `Shutdown
          end else begin
            let n = min (Cstruct.len avail) (Cstruct.len buffer) in
            Cstruct.blit buffer 0 avail 0 n;
            let seq = Int32.(add seq (of_int n)) in
            Console_ring.Ring.Back.Writer.advance t.ring seq;
            Eventchn.notify t.xe t.evtchn;
            loop after (Cstruct.shift buffer n)
          end
        end in
      loop after buffer >>= function
      | `Next after -> read_the_flow after
      | `Shutdown -> return () in
    let (a: unit Lwt.t) =
      let open Lwt in
      catch
        (fun () -> read_the_ring A.program_start)
        (fun e -> printf "read_the_ring caught %s\n%!" (Printexc.to_string e); return ())
      >>= fun () ->
      printf "read_the_ring finished\n%!";
      return () in
    let (b: unit Lwt.t) =
      let open Lwt in
      catch
        (fun () -> read_the_flow A.program_start)
        (fun e -> printf "read_the_flow caught %s\n%!" (Printexc.to_string e); return ())
      >>= fun () ->
      printf "read_the_flow finished\n%!";
      return () in

    let (_: unit Lwt.t) =
      let open Lwt in
      Lwt.join [a; b]
      >>= fun () ->
      printf "Releasing resources\n%!";
      unbind_fn ();
      return () in

    let t, _ = Lwt.task () in
    on_cancel t (fun () ->
      printf "Cancelling ring service_threads\n%!";
      request_shutdown ()
    );
    t

  let init xg xe domid ring_info c =
    let evtchn = Eventchn.bind_interdomain xe domid ring_info.RingInfo.event_channel in
    let grant = { Gnttab.domid = domid; ref = Int32.to_int ring_info.RingInfo.ref } in
    match Gnttab.mapv xg [ grant ] true with
    | None ->
      failwith "Gnttab.mapv failed"
    | Some mapping ->
      let ring = Io_page.to_cstruct (Gnttab.Local_mapping.to_buf mapping) in
      let t = { domid; xg; xe; evtchn; ring } in
      let stats = { total_read = 0; total_write = 0 } in
      let unbind_fn () =
        printf "Gnttab.unmap_exn\n%!";
        let () = Gnttab.unmap_exn xg mapping in ();
        Eventchn.unbind xe evtchn in
      let t = service_thread t c stats unbind_fn in
      t, stats

  open X

  let get_my_domid () =
    immediate (
      let open M in
      read "domid"
      >>= function
      | Some domid -> return (int_of_string domid)
      | None -> return 0
    )

  let mk_backend_path backend_name (domid,devid) =
    get_my_domid ()
    >>= fun self ->
    return (Printf.sprintf "/local/domain/%d/backend/%s/%d/%d" self backend_name domid devid)

  let mk_frontend_path (domid,devid) =
    return (Printf.sprintf "/local/domain/%d/device/console/%d" domid devid)

  let writev pairs =
    transaction (
      let open M in
      let rec loop = function
      | (k, v) :: remaining ->
        write k v
        >>= fun () ->
        loop remaining
      | [] ->
        return () in
      loop pairs
    )

  let readv path keys =
    immediate (
      let open M in
      let rec loop acc = function
      | k :: remaining ->
        read (Filename.concat path k)
        >>= (function
        | None -> loop acc remaining
        | Some v -> loop ((k, v) :: acc) remaining)
      | [] ->
        return acc in
      loop [] keys
    )

  let read_one k = immediate (
    let open M in
      read k
      >>= function
      | None -> return (`Error ("failed to read: " ^ k ^ " (enoent)"))
      | Some v -> return (`OK v)
    )

  let write_one k v = immediate (write k v)

  let exists k =
    read_one k
    >>= function
    | `OK _ -> return true
    | `Error _ -> return false

  (* Request a hot-unplug *)
  let request_close backend_name (domid, devid) =
    lwt backend_path = mk_backend_path backend_name (domid,devid) in
    writev (List.map (fun (k, v) -> backend_path ^ "/" ^ k, v) (Conproto.State.to_assoc_list Conproto.State.Closing))

  let force_close (domid, device) =
    lwt frontend_path = mk_frontend_path (domid, device) in
    write_one (frontend_path ^ "/state") (Conproto.State.to_string Conproto.State.Closed)

  let run backend_name (domid,devid) t =
    let xg = Gnttab.interface_open () in
    let xe = Eventchn.init () in

    let open ConsoleError in
    let ( >>= ) = Lwt.bind in
    lwt backend_path = mk_backend_path backend_name (domid,devid) in

    try_lwt

      ( read_one (backend_path ^ "/frontend") >>= function
        | `Error x -> fail (Failure x)
        | `OK x -> return x ) >>= fun frontend_path ->

      (* wait for the frontend to enter state Initialised *)
      wait (
        let open M in
        read (frontend_path ^ "/" ^ Conproto.State._state)
        >>= function
        | None -> return `Retry
        | Some state ->
          if Conproto.State.of_string state = Some Conproto.State.Initialised
          || Conproto.State.of_string state = Some Conproto.State.Connected
          then return (`Ok ())
          else return `Retry
      ) >>= (function
      | `Error x -> fail x
      | `Ok () -> return ()
      ) >>= fun () ->

      lwt frontend = readv frontend_path Conproto.RingInfo.keys in
      let ring_info = match Conproto.RingInfo.of_assoc_list frontend with
        | `OK x -> x
        | `Error x -> failwith x in

      let be_thread, stats = init xg xe domid ring_info t in
      lwt () = writev (List.map (fun (k, v) -> backend_path ^ "/" ^ k, v) (Conproto.State.to_assoc_list Conproto.State.Connected)) in
      (* wait for the frontend to disappear or enter a Closed state *)
      wait (
        let open M in
          read (frontend_path ^ "/state")
          >>= function
          | None ->
            return (`Ok ())
          | Some state ->
            if Conproto.State.of_string state <> (Some Conproto.State.Closed)
            then return `Retry
            else return (`Ok ())
      ) >>= (function
      | `Error x -> fail x
      | `Ok () -> return ()
      ) >>= fun () ->
      Lwt.cancel be_thread;
      Lwt.return stats
    with e ->
      printf "conback caught %s\n%!" (Printexc.to_string e);
      fail e

  let create ?backend_domid ?name backend_name (domid, device) =
    (* Construct the device: *)
    lwt backend_path = mk_backend_path backend_name (domid, device) in
    lwt frontend_path = mk_frontend_path (domid, device) in
    lwt backend_domid = match backend_domid with
    | None -> get_my_domid ()
    | Some x -> return x in
    let c = Conproto.Connection.({
      virtual_device = device;
      backend_path;
      backend_domid;
      frontend_path;
      frontend_domid = domid;
      protocol = Conproto.Protocol.Vt100;
      name = name;
    }) in
    transaction (
      let open M in
      let rec loop = function
      | (acl, (k, v)) :: remaining ->
        write k v
        >>= fun () ->
        setperms k acl
        >>= fun () ->
        loop remaining
      | [] ->
        return () in
      loop (Conproto.Connection.to_assoc_list c)
    )

  let destroy backend_name (domid, device) =
    lwt backend_path = mk_backend_path backend_name (domid, device) in
    lwt frontend_path = mk_frontend_path (domid, device) in
    immediate (
      let open M in
      rm backend_path
      >>= fun () ->
      rm frontend_path
    )

  let find_free_devid domid =
    Lwt.catch
      (fun () ->immediate (directory (Printf.sprintf "/local/domain/%d/device/console" domid)))
      (function Xenstore.Protocol.Enoent _ -> return [] | e -> fail e)
    >>= fun used ->
    let free =
      used
      |> List.map (fun x -> try Some (int_of_string x) with _ -> None)
      |> List.fold_left (fun acc this -> match this with Some x -> x :: acc | None -> acc) []
      |> List.fold_left max 0 (* console 0 is handled specially *)
      |> (fun x -> x + 1) in
    return free
end


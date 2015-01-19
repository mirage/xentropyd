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

(* To prevent a configuration error where a different channel is connected
   and the guest interprets the data as entropy, the guest will first say
   to us: (as one contiguous write) *)
let handshake_message = "Hello, may I have some entropy?\r\n"
(* and then we will respond with: *)
let handshake_response = "You may treat everything following this message as entropy.\r\n"

module Make(C: V1.CLOCK) = struct

  type 'a io = 'a Lwt.t

  (* We never close this file descriptor *)
  let dev_random_t =
    Lwt_unix.openfile "/dev/urandom" [ Lwt_unix.O_RDONLY ] 0

  type buffer = Cstruct.t

  type flow = {
    fd: Lwt_unix.file_descr;
    buffer: Cstruct.t;
    (* true when we have received the handshake_mesage *)
    mutable handshake_received: bool;
    handshake_received_c: unit Lwt_condition.t;
    (* true when we have responded and are now transmitting entropy *)
    mutable handshake_complete: bool;
    (* We limit the amount of data that can be read in a time period *)
    mutable no_more_reading_until: float;
    period_ms: int;
  }

  let create ~max_bytes ~period_ms () =
    dev_random_t
    >>= fun fd ->
    let buffer = Cstruct.create max_bytes in
    let handshake_received = false in
    let handshake_received_c = Lwt_condition.create () in
    let handshake_complete = false in
    let no_more_reading_until = 0. in
    return { fd; buffer; handshake_received; handshake_received_c; handshake_complete;
             no_more_reading_until; period_ms }

  type error = [ `Unimplemented | `Handshake_failed ]

  let error_message = function
  | `Unimplemented -> "Unimplemented"
  | `Handshake_failed -> "Handshake failed"

  let read flow =
    let rec handshake () =
      if flow.handshake_received then begin
        Printf.fprintf stderr "Sending handshake response.\n%!";
        let buffer = Cstruct.create (String.length handshake_response) in
        Cstruct.blit_from_string handshake_response 0 buffer 0 (Cstruct.len buffer);
        flow.handshake_complete <- true;
        return (`Ok buffer)
      end else
        Lwt_condition.wait flow.handshake_received_c
        >>= fun () ->
        handshake () in
    let rec wait () =
      let time = C.time () in
      let tosleep = flow.no_more_reading_until -. time in
      if tosleep <= 0.
      then return ()
      else
        Lwt_unix.sleep tosleep
        >>= fun () ->
        wait () in
    if not flow.handshake_complete
    then handshake ()
    else begin
      wait ()
      >>= fun () ->
      Lwt_cstruct.complete (Lwt_cstruct.read flow.fd) flow.buffer
      >>= fun () ->
      flow.no_more_reading_until <- (C.time ()) +. (float_of_int flow.period_ms /. 1000.0);
      return (`Ok flow.buffer)
    end

  let write flow buffer =
    if not flow.handshake_received then begin
      let received = Cstruct.to_string buffer in
      if received <> handshake_message then begin
        Printf.fprintf stderr "Received [%s](%d bytes) instead of handshake message.\n%!" (String.escaped received) (Cstruct.len buffer);
        return (`Error `Handshake_failed)
      end else begin
        flow.handshake_received <- true;
        Lwt_condition.broadcast flow.handshake_received_c ();
        return (`Ok ())
      end
    end else begin
      Printf.fprintf stderr "Discarding %d bytes read from the channel.\n%!" (Cstruct.len buffer);
      return (`Ok ())
    end
  
  let writev _ _ = return (`Error `Unimplemented)

  let close _ = return ()
end

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
let debug fmt = Logging.debug "xs" fmt
let error fmt = Logging.error "xs" fmt

module RF = RandomFlow

(* The client provides read/write access to Xenstore *)
module Client = Xenstore.Client.Make(Userspace)
(* We discover when domains have been created or destroyed by watching
   for @introduceDomains and @releaseDomains *)
module WatchEvents = Xenstore.IntroduceDomain.Make(Client)
(* We convert the watch events into events which carry the domids *)
module DomainEvents = Xenstore.DomainWatch.Make(WatchEvents)(Domains)

let connections = Hashtbl.create 37

let start domid =
  debug "Created %d" domid;
  Hashtbl.add connections domid ()

let stop domid =
  debug "Destroyed %d" domid;
  Hashtbl.remove connections domid

let main common daemon max_bytes period_ms =
  if daemon
  then Lwt_daemon.daemonize ~syslog:true ();

  let rec loop state =
    DomainEvents.next state
    >>= fun (events, state) ->
    List.iter (function
    | `Created domid ->
      start domid
    | `Destroyed domid ->
      stop domid
    ) events;
    loop state in
  (* Shut everything down if we get a signal *)
  let rec logging_thread () =
      Logging.(get logger) >>= function
      | [] -> return ()
      | lines ->
        Lwt_list.iter_s
          (fun x ->
             Lwt_io.write Lwt_io.stderr x >>= fun () ->
             Lwt_io.write Lwt_io.stderr "\n"
          ) lines
        >>= fun () ->
        logging_thread () in
  let t = logging_thread () in
  let shutdown_signal _ =
    Hashtbl.iter (fun domid _ -> stop domid) connections;
    Logging.(shutdown logger);
    debug "Shutting down";
    (* Actually exit after the buffers have been flushed *)
    let _ = t >>= fun () -> exit 0; return () in
    () in
  List.iter
    (fun signal ->
      let (_: Lwt_unix.signal_handler_id) = Lwt_unix.on_signal signal shutdown_signal in
      ()
    ) [ Sys.sigint; Sys.sigterm ];

  Lwt_main.run (loop DomainEvents.initial)

let _ = CommandLine.run main

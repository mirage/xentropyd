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

(* The client provides read/write access to Xenstore *)
module Client = Xenstore.Client.Make(Userspace)
(* We discover when domains have been created or destroyed by watching
   for @introduceDomains and @releaseDomains *)
module WatchEvents = Xenstore.IntroduceDomain.Make(Client)
(* We convert the watch events into events which carry the domids *)
module DomainEvents = Xenstore.DomainWatch.Make(WatchEvents)(Domains)
(* We read random data from the system with a rate-limter *)
module RandomFlow = RandomFlow.Make(Clock)
(* We offer the random data to VMs over the console protocol *)
module ConsoleServer = Conback.Make(Unix_activations)(Client)(RandomFlow)

(* The name of the console which will receive the data *)
let name = "org.openmirage.entropy.1"

open Lwt
let debug fmt = Logging.debug "xs" fmt
let error fmt = Logging.error "xs" fmt

let connections = Hashtbl.create 37

let backend_name = "xentropyd"

let start ~max_bytes ~period_ms domid =
  if Hashtbl.mem connections domid
  then return ()
  else begin
    debug "Noticed domain %d, will provide up to %d bytes in %d milliseconds" domid max_bytes period_ms;
    RandomFlow.create ~max_bytes ~period_ms ()
    >>= fun random ->
    ConsoleServer.find_free_devid domid
    >>= fun devid ->
    debug "Domain %d has free console with devid %d" domid devid;
    Client.(immediate (read "domid")) >>= function
    | None ->
      error "Failed to read my own domid from Xenstore.";
      return ()
    | Some backend_domid ->
      let backend_domid = int_of_string backend_domid in
      ConsoleServer.create ~name ~backend_domid backend_name (domid, devid)
      >>= fun () ->
      debug "Created connection to %d.%d" domid devid;
      let _ =
        ConsoleServer.run backend_name (domid, devid) random
        >>= fun stats ->
        debug "Connection %d.%d bytes read: %d; bytes written: %d" domid devid
          stats.Conback.total_read stats.Conback.total_write;
        return () in
      Hashtbl.add connections domid (domid, devid);
      return ()
  end

let stop domid =
  if Hashtbl.mem connections domid then begin
    debug "Shutting down connection to domain %d" domid;
    let c = Hashtbl.find connections domid in
    Hashtbl.remove connections domid;
    debug "Destroying connection to %d.%d" (fst c) (snd c);
    ConsoleServer.destroy backend_name c
  end else return ()

let main common daemon max_bytes period_ms =
  if daemon
  then Lwt_daemon.daemonize ~syslog:true ();

  let rec loop state =
    DomainEvents.next state
    >>= fun (events, state) ->
    Lwt_list.iter_s (function
    | `Created domid ->
      start ~max_bytes ~period_ms domid
    | `Destroyed domid ->
      stop domid
    ) events
    >>= fun () ->
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
    Hashtbl.iter (fun domid _ -> Lwt_main.run (stop domid)) connections;
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

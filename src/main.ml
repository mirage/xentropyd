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

module RF = RandomFlow

let project_url = "http://github.com/djs55/xentropyd"

(* The client provides read/write access to Xenstore *)
module Client = Xenstore.Client.Make(Userspace)
(* We discover when domains have been created or destroyed by watching
   for @introduceDomains and @releaseDomains *)
module WatchEvents = Xenstore.IntroduceDomain.Make(Client)
(* We convert the watch events into events which carry the domids *)
module DomainEvents = Xenstore.DomainWatch.Make(WatchEvents)(Domains)

open Lwt

let main () =
  let rec loop state =
    DomainEvents.next state
    >>= fun (events, state) ->
    List.iter (function
    | `Created domid -> Printf.printf "Created %d\n" domid
    | `Destroyed domid -> Printf.printf "Destroyed %d\n" domid
    ) events;
    flush stdout;
    loop state in
  loop DomainEvents.initial

let _ = Lwt_main.run (main ())

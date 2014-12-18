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
open Sexplib.Std
open Lwt
open S

let debug fmt = Logging.debug "introduceDomain" fmt
let error fmt = Logging.error "introduceDomain" fmt

type domid = int

module Make(C: CLIENT)= struct

  type 'a t = 'a C.t
  let ( >>= ) = C.( >>= )
  let fail = C.fail
  let return = C.return

  type data = unit

  (* When a relevant watch happens we need this counter to be
     changed (at least once) and the conditional variable signalled *)
  let counter = ref 0
  let c = Lwt_condition.create ()

  let watch_cb _ =
    incr counter;
    Lwt_condition.broadcast c ()

  type state' = {
    introduce: C.LowLevel.callback;
    release: C.LowLevel.callback;
    connection: C.LowLevel.connection;
    mutable last_counter_seen: int;
  }

  type state =
  | Connected of state'
  | Disconnected

  let initial = Disconnected

  let rec next = function
  | Disconnected ->
    C.LowLevel.make ()
    >>= fun connection ->
    (* We watch both introduce and release to generate events when domains are
       created and destroyed *)
    C.LowLevel.add_watch_callback connection "@introduceDomain" watch_cb
    >>= fun introduce ->
    C.LowLevel.add_watch_callback connection "@releaseDomain" watch_cb
    >>= fun release ->
    let last_counter_seen = 0 in
    return ((), Connected { introduce; release; connection; last_counter_seen })
  | Connected state when state.last_counter_seen < !counter ->
    return ((), Connected { state with last_counter_seen = !counter })
  | state ->
    Lwt_condition.wait c
    >>= fun () ->
    next state
end

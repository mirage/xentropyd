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

module Make(C: V1.CLOCK) = struct

  type 'a io = 'a Lwt.t

  let dev_random_t =
    Block.connect "/dev/random"
    >>= function
    | `Ok t -> return t
    | `Error _ -> fail (Failure "Failed to open /dev/random")

  type buffer = Cstruct.t

  type flow = {
    device: Block.t;
    buffer: Cstruct.t;
    (* We limit the amount of data that can be read in a time period *)
    mutable no_more_reading_until: float;
    period_ms: int;
  }

  let create ~max_bytes ~period_ms () =
    dev_random_t
    >>= fun device ->
    let buffer = Cstruct.create max_bytes in
    let no_more_reading_until = 0. in
    return { device; buffer; no_more_reading_until; period_ms }

  type error = Block.error

  let read flow =
    let rec wait () =
      let time = C.time () in
      if time > flow.no_more_reading_until
      then return ()
      else
        Lwt_unix.sleep (flow.no_more_reading_until -. time)
        >>= fun () ->
        wait () in
    wait ()
    >>= fun () ->
    Block.read flow.device 0L [ flow.buffer ]
    >>= function
    | `Error _ ->
      return (`Error (`Unknown "Error from Block.read"))
    | `Ok () ->
      flow.no_more_reading_until <- (C.time ()) +. (float_of_int flow.period_ms /. 1000.0);
      return (`Ok flow.buffer)

  let write _ _ = return (`Error `Unimplemented)
  let writev _ _ = return (`Error `Unimplemented)

  let close { device } = Block.disconnect device
end

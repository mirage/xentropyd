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


module Make(Reader: S.READABLE
  with type position = int64
  and type item = Cstruct.t) : sig
  (** Create a buffered STREAM intended for reading on top of an unbuffered
      one *)

  include S.READABLE
    with type position = int64
    and type item = Cstruct.t

  val attach: Reader.stream -> Cstruct.t -> stream
  (** [attach reader buffer] return a buffered reader layered on top of
      [reader]. Data read from the underlying reader will be cached in
      [buffer]. In the case of xenstore, the ring contains 1024 readable
      bytes but the maximum packet size is much larger. If [buffer] is of
      size at least xenstore_payload_max then it will be possible to
      unmarshal whole packets from buffer.

      This call does not initialise [buffer]. *)

  val create: Reader.stream -> Cstruct.t -> stream
  (** [create reader buffer] return a buffered reader layered on top of
      [reader]. Initialises the [buffer]. *)
end

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

module Make(Space: S.READABLE
  with type position = int64
  and type item = Cstruct.t) : sig
  (** Create a buffered STREAM intended for Writing on top of an unbuffered
      one *)

  include S.READABLE
    with type position = int64
    and type item = Cstruct.t

  val attach: Space.stream -> Cstruct.t -> stream
  (** [attach stream buffer] return a buffered READABLE layered on top of
      [stream]. Data buffers read from here will be buffered, and only flushed
      to the underlying stream when [advance] is called.

      This call does not initialise [buffer]. *)

  val create: Space.stream -> Cstruct.t -> stream
  (** [create stream buffer] return a buffered READABLE layered on top of
      [stream]. Initialises the [buffer]. *)

end

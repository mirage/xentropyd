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

val debug: string -> ('b, unit, string, unit) format4 -> 'b
val info: string -> ('b, unit, string, unit) format4 -> 'b
val warn: string -> ('b, unit, string, unit) format4 -> 'b
val error: string -> ('b, unit, string, unit) format4 -> 'b

type logger
(** An in-memory non-blocking logger with a fixed size circular buffer.
    If the buffer is full then some messages may be dropped. The logger
    will replace runs of dropped messages with a single message with
    a count of how many messages were dropped. *)

val logger: logger
(** The xenstore logger *)

val get: logger -> string list Lwt.t
(** [get logger] returns any available log lines or, if none are available,
    blocks until some are available. *)

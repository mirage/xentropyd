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
open Sexplib

type 'a result = [
  | `Ok of 'a
  | `Error of string
]

module type STRINGABLE = sig
  type t
  val to_string: t -> string
  val of_string: string -> t
end

module type SEXPABLE = sig
  type t
  val sexp_of_t: t -> Sexp.t
  val t_of_sexp: Sexp.t -> t
end

module type MARSHALABLE = sig
  type t
  val marshal: t -> Cstruct.t -> Cstruct.t
end

module type INTROSPECTABLE = sig
  type t
  val ls: t -> string list -> string list
  val read: t -> string list -> string option
  val write: t -> string list -> string -> bool
end

module type MONAD = sig
  type 'a t
  val return: 'a -> 'a t
  val ( >>= ): 'a t -> ('a -> 'b t) -> 'b t
end

module type IO = sig
  include MONAD with type 'a t = 'a Lwt.t
  val fail: exn -> 'a t
end

module type EVENTS = sig
  include IO

  type state

  val initial: state

  type data

  val next: state -> (data * state) t
end

module type STREAM = sig
  (** A stream of items. *)

  include IO

  type stream
  (** The stream *)

  type item
  (** The stream consists of a sequence of items *)

  type position with sexp
  (** Each item has a position. The stream itself remains at a fixed position so
      that repeated calls to [read] or [write] process the same data.
      To advance the stream call [advance new_position] *)

  val advance: stream -> position -> unit t
  (** [advanced buf position] declares that we have processed all data up to
      [position] and therefore any buffers may be recycled. *)
end

module type READABLE = sig
  (** A stream of readable items *)

  include STREAM

  val read: stream -> (position * item result) t
  (** [read stream] returns the item at the current stream position. Note this
      function does not advance the stream, so repeated calls should return
      the same data.
      To advance the stream, call [advance position]. *)

end

module type WRITABLE = sig
  (** A stream of writable items *)

  include STREAM

  val write: stream -> item -> position t
  (** [write stream item] writes a packet to the output at the current position
      and returns the next [position] value. This function does not advance
      the stream, so multiple calls will write at the same position.
      To advance the stream, call [advance position] *)
end

module type ACTIVATIONS = sig
  include IO

  type channel
  (** An entity which receives events, which we can wait for *)

  type event
  (** An individual event notification *)

  val program_start: event
  (** represents an event which 'fired' when the program started *)

  val after: channel -> event -> event t
  (** [next channel event] blocks until the system receives an event
      newer than [event] on channel [channel]. If an event is received
      while we aren't looking then this will be remembered and the
      next call to [after] will immediately unblock. If the system
      is suspended and then resumed, all event channel bindings are invalidated
      and this function will fail with Generation.Invalid *)
end

module type DOMAIN_STATE = sig
  type t = {
    domid: int;     (** unique id for a given domain *)
    dying: bool;    (** the domain is being cleaned up *)
    shutdown: bool; (** the domain has stopped running *)
  }
  (** The state of a domain *)

  val list: unit -> t list
  (** [list ()] returns a list of known domains *)
end

module type FOREIGN_PAGE_MAPPER = sig
  val map: int -> nativeint -> Cstruct.t
  (** [map domid mfn] maps a foreign page *)

  val unmap: Cstruct.t -> unit
  (** [unmap page] unmaps a previously-mapped foreign page *)
end

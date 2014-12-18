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

(** XenStore protocol. *)

type ('a, 'b) result = [
  | `Ok of 'a
  | `Error of 'b
]

val xenstore_payload_max: int
(** the maximum size of a xenstore packet payload (not including header) *)

module Op : sig
  type t =
    | Debug
    | Directory
    | Read
    | Getperms
    | Watch
    | Unwatch
    | Transaction_start
    | Transaction_end
    | Introduce
    | Release
    | Getdomainpath
    | Write
    | Mkdir
    | Rm
    | Setperms
    | Watchevent
    | Error
    | Isintroduced
    | Resume
    | Set_target
    | Restrict
    | Reset_watches
  with sexp
  (** The type of xenstore operation. *)

  val to_string: t -> string
  val of_string: string -> t

  val all: t list
  (** All known operations *)

  val of_int32: int32 -> (t, string) result
  (** Map an int32 onto a [t]. If no mapping exists then the best we can do
      is log the result string and close the connection. *)

  val to_int32: t -> int32
end

module Header : sig
  type t = {
    tid: int32; (** transaction id *)
    rid: int32; (** request id: for matching replies *)
    ty: Op.t;   (** the type of the payload *)
    len: int;   (** the length of the payload *)
  } with sexp
  (** Every xenstore message starts with a fixed-length header *)

  val sizeof: int
  (** The size of the header in bytes *)

  val marshal: t -> Cstruct.t -> Cstruct.t
  (** [marshal t buf] writes [t] to [buf], and returns [buf] shifted along by
      the size of [t] *)

  val unmarshal: Cstruct.t -> (t, string) result
  (** [unmarshal buf] reads a [t] from [buf], or produces a descriptive error
      message. *)
end

module Token : sig
  type t
  (** A token is associated with every watch and returned in the
      callback. *)

  val to_debug_string: t -> string
  (** [to_string token] is a debug-printable version of [token]. *)

  val to_user_string: t -> string
  (** [to_user_string token] is the user-supplied part of [token]. *)

  val unmarshal: string -> t
  (** [of_string str_rep] is the token resulting from the
      unmarshalling of [str_rep]. *)

  val marshal: t -> string
  (** [to_string token] is the marshalled representation of [token]. *)
end

module Path : sig
  module Element : sig
    type t with sexp
    (** an element of a path *)

    exception Invalid_char of char

    val of_string: string -> t
    (** [of_string x] returns a [t] which corresponds to [x], or
        raises Invalid_char *)

    val to_string: t -> string
    (** [to_string t] returns a string which corresponds to [t] *)
  end

  type t with sexp
  (** a sequence of elements representing a 'path' from one node
      in the store down to another *)

  val empty: t
  (** the empty path *)

  exception Invalid_path of string * string
  (** [Invalid_path (path, reason)] indicates that [path] is invalid
      because [reason] *)

  val of_string: string -> t
  (** [of_string x] returns the [t] associated with [x], or raises
      Invalid_path *)

  val to_list: t -> Element.t list
  (** [to_list t] returns [t] as a list of elements *)

  val to_string: t -> string
  (** [to_string t] returns [t] as a string *)

  val to_string_list: t -> string list
  (** [to_string_list t] returns [t] as a string list *)

  val of_string_list: string list -> t
  (** [of_string_list x] parses [x] and returns a [t] *)

  val dirname: t -> t
  (** [dirname t]: returns the parent path of [t], or [t] itself if there is
      no parent, cf Filename.dirname *)

  val basename: t -> Element.t
  (** [basename t]: returns the final element of [t], cf Filename.basename *)

  val concat: t -> t -> t
  (** [concat a b] returns the path given by [b] joined onto the end of [a] *)

  val walk: (Element.t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [walk f t]: folds [f] across each path element of [t] in order *)

  val fold: (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t initial]: folds [f] across each prefix sub-path of [t] in
      order of increasing length *)

  val iter: (t -> unit) -> t -> unit
  (** [iter f t]: applies every prefix sub-path of [t] to [f] in order of
      increasing length *)

  val common_prefix: t -> t -> t
  (** [common_prefix a b] returns the common prefix of [a] and [b] *)
end

module Name : sig
  type predefined =
    | IntroduceDomain
    | ReleaseDomain
  with sexp

  type t =
    | Predefined of predefined
    | Absolute of Path.t
    | Relative of Path.t
  with sexp
  (** a Name.t refers to something which can be watched, read or
      written via the protocol. *)

  val of_string: string -> t
  (** [of_string x] converts string [x] into a [t], or raises Invalid_path *)

  val to_string: t -> string
  (** [to_string t] converts [t] into a string *)

  val is_relative: t -> bool
  (** [is_relative t] is true if [t] is relative to a connection's directory *)

  val resolve: t -> t -> t
  (** [resolve t relative_to] interprets [t] relative to directory [relative_to].
      If [t] is relative and [relative_to] absolute, the result is absolute.
      In all other cases [t] is returned as-is. *)

  val relative: t -> t -> t
  (** [relative t base]: if [t] and [base] are absolute and [base] is a prefix
      of [t], return a relative path which refers to [t] when resolved
      relative to [base]. *)

  val to_path: t -> Path.t
  (** [to_path t]: if [t] is an Absolute or Relative path, return it. Otherwise
      raise Invalid_path *)
end

module ACL : sig

  type perm =
    | NONE  (** no permission *)
    | READ  (** read only *)
    | WRITE (** write only *)
    | RDWR  (** read and write *)
  with sexp
  (** An access control list grants permissions to domains. *)

  val char_of_perm: perm -> char
  (** Each perm is associated with a char in the normal UI *)

  type domid = int with sexp

  type t = {
    owner: domid;             (** domain which "owns", has full access *)
    other: perm;              (** default permissions for all others... *)
    acl: (domid * perm) list; (** ... unless overridden in the ACL *)
  } with sexp
  (** an access control list *)

  val to_string: t -> string
  (** Print an ACL in the same format as 'xenstore-ls' *)

  val unmarshal: Cstruct.t -> (t, string) result
  (** [unmarshal buf] reads a [t] from [buf] or produces a descriptive error
      message. *)

  val marshal: t -> Cstruct.t -> Cstruct.t
  (** [marshal t buf] writes [t] to [buf] and returns [buf] shifted along by
      the size of [t] *)
end
(** Access control lists. *)

module Response : sig
  type t =
    | Read of string
    | Directory of string list
    | Getperms of ACL.t
    | Getdomainpath of string
    | Transaction_start of int32
    | Write
    | Mkdir
    | Rm
    | Setperms
    | Watch
    | Unwatch
    | Transaction_end
    | Debug of string list
    | Introduce
    | Resume
    | Release
    | Set_target
    | Restrict
    | Isintroduced of bool
    | Error of string
    | Watchevent of Name.t * string
    | Reset_watches
  with sexp
  (** the body of a response *)

  val to_string: t -> string
  (** [to_string t] returns a short human-readable description of [t] *)

  val get_ty: t -> Op.t
  (** [get_ty t] returns the operation code corresponding to [t] *)

  val unmarshal: Header.t -> Cstruct.t -> (t, string) result
  (** [unmarshal header payload] parses [payload] according to the
      operation code in [header] and returns a [t] *)

  val marshal: t -> Cstruct.t -> Cstruct.t
  (** [marshal t buf] writes [t] to [buf] and shifts [buf] along by the
      size of [t] *)
end

module Request : sig

  type path_op =
    | Read
    | Directory
    | Getperms
    | Write of string
    | Mkdir
    | Rm
    | Setperms of ACL.t
  with sexp

  type t =
    | PathOp of string * path_op
    | Getdomainpath of int
    | Transaction_start
    | Watch of string * string
    | Unwatch of string * string
    | Transaction_end of bool
    | Debug of string list
    | Introduce of int * Nativeint.t * int
    | Resume of int
    | Release of int
    | Set_target of int * int
    | Restrict of int
    | Isintroduced of int
    | Reset_watches
  with sexp
  (** the payload of a request *)

  val to_string: t -> string
  (** [to_string t] returns a short human-readable description of [t] *)

  val get_ty: t -> Op.t
  (** [get_ty t] returns the operation code associated with [t] *)

  val unmarshal: Header.t -> Cstruct.t -> (t, string) result
  (** [unmarshal header payload] parses [payload] according to the
      operation code in [header] and returns a [t] *)

  val marshal: t -> Cstruct.t -> Cstruct.t
  (** [marshal t buf] writes [t] to [buf] and returns [buf] shifted by
      the sizeof [t] *)
end

exception Enoent of string (** Raised when a named key does not exist. *)
exception Eagain           (** Raised when a transaction must be repeated. *)
exception Invalid
exception Error of string  (** Generic catch-all error. *)

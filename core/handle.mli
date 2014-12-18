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

type 'a t
(** A 'handle' is a sub-connection used for a particular purpose.
    The handle is a convenient place to store sub-connection state *)

val make: 'a -> 'a t

val get_tid: 'a t -> int32
(** return the transaction id (typically for debug printing) *)

val get_client: 'a t -> 'a
(** return the client instance wrapped in a handle *)

val no_transaction: 'a -> 'a t
(** Handle used for 'immediate' non-transactional read/writes *)

val transaction: 'a -> int32 -> 'a t
(** Handle used for transactional read/writes *)

val watching: 'a -> 'a t
(** Handle used to store watch-related information *)

val accessed_path: 'a t -> string -> 'a t
(** Get the list of recorded path accesses *)

module StringSet : module type of Set.Make(struct type t = string let compare = compare end)

val get_accessed_paths: 'a t -> StringSet.t
(** Get the list of paths we have accessed *)

val watch: 'a t -> string -> 'a t
(** Declare that we are watching a path *)

val unwatch: 'a t -> string -> 'a t
(** Declare that we are nolonger watching a path *)

val get_watched_paths: 'a t -> StringSet.t
(** Get the list of paths we're currently watching *)


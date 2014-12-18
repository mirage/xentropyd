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

module StringSet = Set.Make(struct type t = string let compare = compare end)

type 'a t = {
  client: 'a;
  tid: int32;                                 (* transaction id in use (0 means no transaction) *)
  mutable accessed_paths: StringSet.t option; (* paths read or written to *)
  mutable watched_paths: StringSet.t;         (* paths being watched *)
}

let make client = {
  client = client;
  tid = 0l;                       (* no transaction *)
  accessed_paths = None;          (* not recording accesses *)
  watched_paths = StringSet.empty (* no paths watched *)
}

let get_tid h = h.tid

let get_client h = h.client

let no_transaction client = make client

let transaction client tid = { (make client) with tid = tid }

let watching client = { (make client) with accessed_paths = Some StringSet.empty }

let accessed_path h path = match h.accessed_paths with
  | None -> h
  | Some ps -> h.accessed_paths <- Some (StringSet.add path ps); h

let get_accessed_paths h = match h.accessed_paths with
  | None -> StringSet.empty
  | Some xs -> xs

let watch h path = h.watched_paths <- StringSet.add path h.watched_paths; h

let unwatch h path = h.watched_paths <- StringSet.remove path h.watched_paths; h

let get_watched_paths h = h.watched_paths



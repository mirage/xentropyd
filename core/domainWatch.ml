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

let debug fmt = Logging.debug "domainWatch" fmt
let error fmt = Logging.error "domainWatch" fmt

type domid = int

module Make(E: EVENTS)(DS: DOMAIN_STATE) = struct

  include IO_lwt
  
  module Domid_Map = Map.Make(struct
    type t = int
    let compare (a: int) (b: int) = compare a b
  end)

  type state = E.state * (DS.t Domid_Map.t)

  let initial = E.initial, Domid_Map.empty

  type data = [ `Created of int | `Destroyed of int ] list

  let next (state, domains) =
    E.next state >>= fun (_, state) ->
    (* Check to see if any of our domains have shutdown *)
    let dis = DS.list () in
    List.iter (fun di ->
      if di.DS.dying || di.DS.shutdown
      then debug "domid %d: %s%s%s" di.DS.domid
        (if di.DS.dying then "dying" else "")
        (if di.DS.dying && di.DS.shutdown then " and " else "")
        (if di.DS.shutdown then "shutdown" else "")
    ) dis;
    let dis_map = List.fold_left (fun acc elt -> Domid_Map.add elt.DS.domid elt acc) Domid_Map.empty dis in
    let to_close = Domid_Map.filter (fun x _ ->
      not(Domid_Map.mem x dis_map)
      || (let di = Domid_Map.find x dis_map in di.DS.shutdown || di.DS.dying)
    ) domains in
    let to_open = Domid_Map.filter (fun x _ ->
      not(Domid_Map.mem x domains)
    ) dis_map in
    let data : data =
         Domid_Map.fold (fun x _ acc -> `Destroyed x :: acc) to_close []
      @ (Domid_Map.fold (fun x _ acc -> `Created x :: acc) to_open []) in
    let state : state = state, dis_map in
    return (data, state)
end

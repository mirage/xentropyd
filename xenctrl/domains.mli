(*
 * Copyright (C) Citrix Systems Inc.
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; version 2.1 only. with the special
 * exception on linking described in file LICENSE.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *)

val map_foreign: int -> nativeint -> Io_page.t
(** [map_foreign domid mfn] maps the page from [domid] with address
    [mfn]. This is typically used to map the xenstore ring. *)

val unmap_foreign: Io_page.t -> unit
(** [unmap_foreign page] unmaps the foreign [page]. Note the domain cannot
    be properly cleaned up by Xen until all foreign maps are released,
    so it's important to call this early. Any further accesses to the
    [page] will trigger a page fault. *)

val map_fd: Unix.file_descr -> int -> Io_page.t
(** [map_fd fd size] memory maps [size] bytes from the open file [fd] *)

include S.DOMAIN_STATE

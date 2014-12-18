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
let max_packet_size = Protocol.xenstore_payload_max + Protocol.Header.sizeof

module Make(Marshal: S.MARSHALABLE)(WriteBuffers: S.READABLE
  with type position = int64
  and type item = Cstruct.t) = struct

  include IO_lwt
  
  type stream = WriteBuffers.stream
  type position = WriteBuffers.position with sexp
  type item = Protocol.Header.t * Marshal.t

  let write t (hdr, m) =
    let rec loop () =
      WriteBuffers.read t >>= function
      | _, `Error x -> fail (Failure x)
      | _, `Ok space ->
        if Cstruct.len space >= max_packet_size
        then return ()
        else loop () in
    loop () >>= fun () ->
    WriteBuffers.read t >>= function
    | _, `Error x -> fail (Failure x)
    | offset, `Ok space ->
      let payload_buf = Cstruct.shift space Protocol.Header.sizeof in
      let next = Marshal.marshal m payload_buf in
      let length = next.Cstruct.off - payload_buf.Cstruct.off in
      let hdr = Protocol.Header.({ hdr with len = length }) in
      ignore (Protocol.Header.marshal hdr space);
      let last_byte = Int64.(add offset (of_int (Protocol.Header.sizeof + length))) in
      return last_byte

  let advance t ofs =
    WriteBuffers.advance t ofs
end

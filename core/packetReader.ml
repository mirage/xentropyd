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
module Make(Unmarshal: S.UNMARSHALABLE)(Reader: S.READABLE
  with type position = int64
  and  type item = Cstruct.t) = struct
  type stream = Reader.stream
  type item = Protocol.Header.t * Unmarshal.t
  type position = Reader.position with sexp
  include IO_lwt

  let rec read t =
    Reader.read t >>= function
    | offset, `Error x -> return (offset, `Error x)
    | offset, `Ok space ->
      let len = Cstruct.len space in
      if len < Protocol.Header.sizeof
      then read t
      else begin
        match Protocol.Header.unmarshal space with
        | `Error x -> return (offset, `Error x)
        | `Ok x ->
          let length = Protocol.Header.sizeof + x.Protocol.Header.len in
          let rec loop space =
            let len = Cstruct.len space in
            if len < length then begin
              Reader.read t >>= function
              | offset, `Error x -> return (offset, `Error x)
              | offset, `Ok space -> loop space
            end else begin
              let payload = Cstruct.sub space Protocol.Header.sizeof x.Protocol.Header.len in
              let offset = Int64.(add offset (of_int length)) in
              match Unmarshal.unmarshal x payload with
              | `Ok body -> return (offset, `Ok (x, body))
              | `Error x -> return (offset, `Error x)
            end in
          loop space
      end

  let advance t ofs =
    Reader.advance t ofs
end

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
module Make(Reader: S.READABLE
  with type position = int64
  and type item = Cstruct.t) = struct

  include IO_lwt

  type position = Reader.position with sexp
  type item = Reader.item

  cstruct hdr {
    uint64_t producer;
    uint64_t consumer;
  } as little_endian

  type stream = {
    stream: Reader.stream;
    output: Cstruct.t;
  }

  let attach stream output = { stream; output }

  let create stream output =
    set_hdr_producer output 0L;
    set_hdr_consumer output 0L;
    attach stream output

  (* read a bit more data from the underlying buffer, return what we have
     so far *)
  let read stream =
    let producer = get_hdr_producer stream.output in
    let consumer = get_hdr_consumer stream.output in
    let buffer = Cstruct.shift stream.output 16 in
    let len = Cstruct.len buffer in

    Reader.read stream.stream >>= function
    | offset, `Error x -> return (offset, `Error x)
    | offset, `Ok space ->
      (* copy as much as possible into our buffer *)
      ( if offset > producer
        then fail (failwith (Printf.sprintf "Some portion of the input stream has been dropped. Our data starts at %Ld, the stream starts at %Ld" producer offset))
        else return () ) >>= fun () ->
      let producer = offset in
      (* total data we can write is len - (producer - consumer) but we need to
         subdivide this into contiguous chunks *)
      let used = Int64.(to_int (sub producer consumer)) in
      let free = len - used in
      let producer_wrapped = Int64.(to_int (rem producer (of_int len))) in
      let to_buffer_end = len - producer_wrapped in
      let contiguous = min free to_buffer_end in
      let n = min (Cstruct.len space) contiguous in
      let to_write = Cstruct.sub buffer producer_wrapped n in
      Cstruct.blit space 0 to_write 0 n;
      let producer = Int64.(add producer (of_int n)) in
      set_hdr_producer stream.output producer;
      Reader.advance stream.stream producer >>= fun () ->

      (* return everything we've got to the user *)
      let consumer_wrapped = Int64.(to_int (rem consumer (of_int len))) in
      let to_buffer_end = len - consumer_wrapped in
      let used = Int64.(to_int (sub producer consumer)) in
      let contiguous = min used to_buffer_end in
      let space = Cstruct.sub buffer consumer_wrapped contiguous in
      return (consumer, `Ok space)

  let advance stream up_to =
    set_hdr_consumer stream.output up_to;
    return ()
end

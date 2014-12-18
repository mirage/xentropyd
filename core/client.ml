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

(** A multiplexing xenstore protocol client over a byte-level transport *)

open Lwt
open Protocol

let debug fmt = Logging.debug "client" fmt
let error fmt = Logging.error "client" fmt

let ( |> ) a b = b a
let ( ++ ) f g x = f (g x)

module StringSet = Handle.StringSet

module Watcher = struct

  (** Someone who is watching paths is represented by one of these: *)
  type t = {
    mutable paths: StringSet.t; (* we never care about events or ordering, only paths *)
    mutable cancelling: bool; (* we need to stop watching and clean up *)
    c: unit Lwt_condition.t;
    m: Lwt_mutex.t;
  }

  let make () = {
    paths = StringSet.empty;
    cancelling = false;
    c = Lwt_condition.create ();
    m = Lwt_mutex.create ();
  }

  (** Register that a watched path has been changed *)
  let put (x: t) path =
    Lwt_mutex.with_lock x.m
      (fun () ->
         x.paths <- StringSet.add path x.paths;
         Lwt_condition.signal x.c ();
         return ();
      )

  (** Return a set of modified paths, or an empty set if we're cancelling *)
  let get (x: t) =
    Lwt_mutex.with_lock x.m
      (fun () ->
         while_lwt x.paths = StringSet.empty && not x.cancelling do
           Lwt_condition.wait ~mutex:x.m x.c
         done >>
         let results = x.paths in
         x.paths <- StringSet.empty;
         return results
      )

  (** Called to shutdown the watcher and trigger an orderly cleanup *)
  let cancel (x: t) =
    let (_: unit Lwt.t) =
      Lwt_mutex.with_lock x.m
        (fun () ->
           x.cancelling <- true;
           Lwt_condition.signal x.c ();
           return ()
        ) in
    ()
end

exception Malformed_watch_event
exception Unexpected_rid of int32
exception Dispatcher_failed

let fail_on_error = function
  | `Ok x -> return x
  | `Error x -> fail (Failure x)

module Make = functor(IO: S.CONNECTION) -> struct

  type 'a t = 'a IO.t
  let ( >>= ) = IO.( >>= )
  let return = IO.return
  let fail = IO.fail

  (* Represents a single acive connection to a server *)
  type client = {
    mutable transport: IO.connection;
    rid_to_wakeup: (int32, Response.t Lwt.u) Hashtbl.t;
    mutable dispatcher_thread: unit Lwt.t;
    mutable dispatcher_shutting_down: bool;
    watchevents: (Token.t, Watcher.t) Hashtbl.t;
    manual_watch_callbacks: (Token.t, (int * (string -> unit)) list) Hashtbl.t;

    mutable suspended : bool;
    suspended_m : Lwt_mutex.t;
    suspended_c : unit Lwt_condition.t;
  }

  let client_cache = ref None
  (* The whole application must only use one xenstore client, which will
     multiplex all requests onto the same ring. *)

  let client_cache_m = Lwt_mutex.create ()
  (* Multiple threads will call 'make' in parallel. We must ensure only
     one client is created. *)

  let handle_exn t e =
    error "Caught fatal exception: %s" (Printexc.to_string e);
    error "I will shut down the xenstore client";
    t.dispatcher_shutting_down <- true; (* no more hashtable entries after this *)
    (* all blocking threads are failed with our exception *)
    lwt () = Lwt_mutex.with_lock t.suspended_m (fun () ->
        error "Propagating exception to %d threads" (Hashtbl.length t.rid_to_wakeup);
        Hashtbl.iter (fun _ u -> Lwt.wakeup_later_exn u e) t.rid_to_wakeup;
        return ()) in
    raise_lwt e

  let rec dispatcher t =
    IO.Response.Reader.read t.transport >>= fun (position, x) ->
    IO.Response.Reader.advance t.transport position >>= fun () ->
    fail_on_error x >>= fun (hdr, payload) ->
    match payload with
    | Response.Watchevent(path, token) ->
      let token = Token.unmarshal token in
      if Hashtbl.mem t.watchevents token then begin
        (* It's one of our automatic watch events from [wait] *)
        Watcher.put (Hashtbl.find t.watchevents token) (Name.to_string path) >>= fun () ->
        dispatcher t
      end else if Hashtbl.mem t.manual_watch_callbacks token then begin
        (* It's one of our manual watch events from the Lowlevel interface *)
        List.iter (fun (_, fn) -> fn (Protocol.Name.to_string path)) (Hashtbl.find t.manual_watch_callbacks token);
        dispatcher t
      end else begin
        (* It's an old watch event that was queued and we're not interested any more *)
        dispatcher t
      end
    | r ->
      begin
        let rid = hdr.Header.rid in
        Lwt_mutex.with_lock t.suspended_m (fun () ->
          if Hashtbl.mem t.rid_to_wakeup rid
          then return (Some (Hashtbl.find t.rid_to_wakeup rid))
          else return None) >>= function
        | None -> handle_exn t (Unexpected_rid rid)
        | Some thread ->
          begin
            Lwt.wakeup_later thread r;
            dispatcher t
          end
      end

  let suspend () =
    match_lwt Lwt_mutex.with_lock client_cache_m (fun () -> return !client_cache) with
    | None -> return ()
    | Some t ->
      Lwt_mutex.with_lock t.suspended_m
        (fun () ->
           t.suspended <- true;
           while_lwt (Hashtbl.length t.rid_to_wakeup > 0) do
             Lwt_condition.wait ~mutex:t.suspended_m t.suspended_c
           done) >>= fun () ->
      Hashtbl.iter (fun _ watcher -> Watcher.cancel watcher) t.watchevents;
      Lwt.cancel t.dispatcher_thread;
      return ()

  let resume_unsafe t =
    lwt () = Lwt_mutex.with_lock t.suspended_m (fun () ->
        t.suspended <- false;
        t.dispatcher_shutting_down <- false;
        Lwt_condition.broadcast t.suspended_c ();
        return ()) in
    t.dispatcher_thread <- dispatcher t;
    return ()

  let resume () =
    match_lwt Lwt_mutex.with_lock client_cache_m (fun () -> return !client_cache) with
    | None -> return ()
    | Some t ->
      IO.create () >>= fun transport ->
      t.transport <- transport;
      resume_unsafe t

  type ctx = client Handle.t

  module M = struct
    type 'a t = ctx -> 'a Lwt.t

    let ( >>= ) f_h next =
       fun h ->
         f_h h >>= fun x ->
         next x h
    let return x h = return x
  end

  let make_rid =
    let counter = ref 0l in
    fun () ->
      let result = !counter in
      counter := Int32.succ !counter;
      result

  let rpc h payload f =
    let open Handle in
    let rid = make_rid () in
    let tid = get_tid h in
    let ty = Request.get_ty payload in
    let t, u = wait () in
    let c = get_client h in
    if c.dispatcher_shutting_down
    then raise_lwt Dispatcher_failed
    else begin
      lwt () = Lwt_mutex.with_lock c.suspended_m (fun () ->
          lwt () = while_lwt c.suspended do
              Lwt_condition.wait ~mutex:c.suspended_m c.suspended_c
            done in
          Hashtbl.add c.rid_to_wakeup rid u;
          let hdr = { Header.rid; tid; ty; len = 0} in
          IO.Request.Writer.write c.transport (hdr, payload) >>= fun position ->
          IO.Request.Writer.advance c.transport position) in
      lwt res = t in
      lwt () = Lwt_mutex.with_lock c.suspended_m
          (fun () ->
             Hashtbl.remove c.rid_to_wakeup rid;
             Lwt_condition.broadcast c.suspended_c ();
             return ()) in
      f res
    end

  let error hint = function
    | Response.Error "ENOENT" -> raise (Enoent hint)
    | Response.Error "EAGAIN" -> raise Eagain
    | Response.Error "EINVAL" -> raise Invalid
    | Response.Error x        -> raise (Error x)
    | x              -> raise (Error (Printf.sprintf "%s: unexpected response: %s" hint (Sexplib.Sexp.to_string_hum (Response.sexp_of_t x))))

  let watch path token h = rpc (Handle.watch h path) (Request.Watch(path, Token.marshal token))
      (function Response.Watch -> return ()
              | x -> error "watch" x)
  let unwatch path token h = rpc (Handle.watch h path) (Request.Unwatch(path, Token.marshal token))
      (function Response.Unwatch -> return ()
              | x -> error "unwatch" x)

  let directory path h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Directory))
      (function Response.Directory ls -> return ls
              | x -> error "directory" x)
  let read_exn path h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Read))
      (function Response.Read x -> return x
              | x -> error "read" x)
  let read path h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Read))
      (function Response.Read x -> return (Some x)
              | Response.Error "ENOENT" -> return None
              | x -> error "read" x)
  let write path data h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Write data))
      (function Response.Write -> return ()
              | x -> error "write" x)
  let rm path h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Rm))
      (function Response.Rm -> return ()
              | x -> error "rm" x)
  let mkdir path h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Mkdir))
      (function Response.Mkdir -> return ()
              | x -> error "mkdir" x)
  let setperms path acl h = rpc (Handle.accessed_path h path) Request.(PathOp(path, Setperms acl))
      (function Response.Setperms -> return ()
              | x -> error "setperms" x)
  let debug cmd_args h = rpc h (Request.Debug cmd_args)
      (function Response.Debug debug -> return debug
              | x -> error "debug" x)
  let restrict domid h = rpc h (Request.Restrict domid)
      (function Response.Restrict -> return ()
              | x -> error "restrict" x)
  let getdomainpath domid h = rpc h (Request.Getdomainpath domid)
      (function Response.Getdomainpath x -> return x
              | x -> error "getdomainpath" x)
  let introduce domid store_mfn store_port h = rpc h (Request.Introduce(domid, store_mfn, store_port))
      (function Response.Introduce -> return ()
              | x -> error "introduce" x)
  let set_target stubdom_domid domid h = rpc h (Request.Set_target(stubdom_domid, domid))
      (function Response.Set_target -> return ()
              | x -> error "set_target" x)

  module LowLevel = struct

    type connection = client

    let make_unsafe () =
      lwt transport = IO.create () in
      let t = {
        transport = transport;
        rid_to_wakeup = Hashtbl.create 10;
        dispatcher_thread = return ();
        dispatcher_shutting_down = false;
        watchevents = Hashtbl.create 37;
        manual_watch_callbacks = Hashtbl.create 37;
        suspended = false;
        suspended_m = Lwt_mutex.create ();
        suspended_c = Lwt_condition.create ();
      } in
      t.dispatcher_thread <- dispatcher t;
      return t

    let make () =
      Lwt_mutex.with_lock client_cache_m
        (fun () -> match !client_cache with
           | Some c -> return c
           | None ->
             lwt c = make_unsafe () in
             client_cache := Some c;
             return c
        )

    type callback = Protocol.Token.t * int

    let next_id =
      let counter = ref 0 in
      fun () ->
        let c = !counter in
        incr counter;
        c

    let add_watch_callback t path fn =
      let token = Token.unmarshal path in
      let existing =
        if Hashtbl.mem t.manual_watch_callbacks token
        then Hashtbl.find t.manual_watch_callbacks token
        else [] in
      let counter = next_id () in
      (* We set the token to be the path *)
      Hashtbl.replace t.manual_watch_callbacks token ((counter, fn) :: existing);
      watch path token (Handle.no_transaction t) >>= fun () ->
      return (token, counter)

    let del_watch_callback t (token, counter) =
       let path = Token.marshal token in
       if Hashtbl.mem t.manual_watch_callbacks token then begin
         let existing = Hashtbl.find t.manual_watch_callbacks token in
         let removed = List.filter (fun (c, _) -> c <> counter) existing in
         if existing = [] then begin
           unwatch path token (Handle.no_transaction t) >>= fun () ->
           Hashtbl.remove t.manual_watch_callbacks token;
           return ()
         end else begin
           Hashtbl.replace t.manual_watch_callbacks token removed;
           return ()
         end
       end else return ()
  end

  let make = LowLevel.make

  let immediate f =
    make () >>= fun client ->
    f (Handle.no_transaction client)

  let counter = ref 0l

  let wait f =
    make () >>= fun client ->
    let open StringSet in
    counter := Int32.succ !counter;
    let token = Token.unmarshal (Printf.sprintf "%ld:xs_client.wait" !counter) in
    (* When we register the 'watcher', the dispatcher thread will signal us when
       watches arrive. *)
    let watcher = Watcher.make () in
    Hashtbl.add client.watchevents token watcher;

    (* We signal the caller via this cancellable task: *)
    let result, wakener = Lwt.task () in
    on_cancel result
      (fun () ->
         (* Trigger an orderly cleanup in the background: *)
         Watcher.cancel watcher
      );
    let h = Handle.watching client in
    (* Adjust the paths we're watching (if necessary) and block (if possible) *)
    let adjust_paths () =
      let current_paths = Handle.get_watched_paths h in
      (* Paths which weren't read don't need to be watched: *)
      let old_paths = diff current_paths (Handle.get_accessed_paths h) in
      lwt () = Lwt_list.iter_s (fun p -> unwatch p token h) (elements old_paths) in
      (* Paths which were read do need to be watched: *)
      let new_paths = diff (Handle.get_accessed_paths h) current_paths in
      lwt () = Lwt_list.iter_s (fun p -> watch p token h) (elements new_paths) in
      (* If we're watching the correct set of paths already then just block *)
      if old_paths = empty && (new_paths = empty)
      then begin
        lwt results = Watcher.get watcher in
        (* an empty results set means we've been cancelled: trigger cleanup *)
        if results = empty
        then fail (Failure "goodnight")
        else return ()
      end else return () in
    (* Main client loop: *)
    let rec loop () =
      f h >>= function
      | `Ok x ->
        wakeup wakener (`Ok x);
        return ()
      | `Error y ->
        wakeup wakener (`Error y);
        return ()
      | `Retry ->
        adjust_paths () >>= fun () ->
        loop () in
    let (_: unit Lwt.t) =
      try_lwt
        loop ()
      finally
      let current_paths = Handle.get_watched_paths h in
      lwt () = Lwt_list.iter_s (fun p -> unwatch p token h) (elements current_paths) in
      Hashtbl.remove client.watchevents token;
      return () in
    result

  let rec transaction f =
    make () >>= fun client ->
    lwt tid = rpc (Handle.no_transaction client) Request.Transaction_start
        (function Response.Transaction_start tid -> return tid
                | x -> error "transaction_start" x) in
    let h = Handle.transaction client tid in
    lwt result = f h in
    try_lwt
      rpc h (Request.Transaction_end true)
        (function Response.Transaction_end -> return result
                | x -> error "transaction_end" x)
    with Eagain ->
      transaction f
end

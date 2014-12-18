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

let project_url = "http://github.com/mirage/xentropyd"

module Common = struct
  type t = {
    verbose: bool;
    debug: bool;
  }
  (** options common to all subcommands *)

  let make verbose debug = { verbose; debug }
end

open Cmdliner

(* Help sections common to all commands *)

let _common_options = "COMMON OPTIONS"
let help = [
 `S _common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command."; `Noblank;
 `S "BUGS"; `P (Printf.sprintf "Check bug reports at %s" project_url);
]

(* Options common to all commands *)
let common_options_t =
  let docs = _common_options in
  let debug =
    let doc = "Give only debug output." in
    Arg.(value & flag & info ["debug"] ~docs ~doc) in
  let verb =
    let doc = "Give verbose output." in
    let verbose = true, Arg.info ["v"; "verbose"] ~docs ~doc in
    Arg.(last & vflag_all [false] [verbose]) in
  Term.(pure Common.make $ debug $ verb)

let command f =
  let doc = "Provide entropy to VMs" in
  let man = [
    `S "DESCRIPTION";
    `P "Watch for domains being created, and connect to them over the console protocol and write entropy values.";
  ] in
  let daemon =
    let doc = "Detach from the terminal and daemonize." in
    Arg.(value & flag & info [ "daemon" ] ~doc) in
  let max_bytes =
    let doc = "The maximum number of bytes of entropy provided to the domain in each time period." in
    Arg.(value & opt int 1024 & info [ "max-bytes" ] ~doc) in
  let period_ms =
    let doc = "The length of each time period in milliseconds." in
    Arg.(value & opt int (60 * 1000) & info [ "period-ms" ] ~doc) in
  Term.(ret (pure f $ common_options_t $ daemon $ max_bytes $ period_ms )),
  Term.info "xentropyd" ~version:"0.1" ~sdocs:_common_options ~doc ~man

let run f =
  match Term.eval (command f) with
  | `Error _ -> exit 1
  | _ -> exit 0

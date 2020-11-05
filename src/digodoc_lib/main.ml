(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

(*
  DONE:
  * find all installed opam packages
  * read changes files to discover ownership of files by opam packages
  * read all META files associated with opam packages
  * associate .cma/.cmxa files to meta packages and opam packages

  TODO:
  * read opam files for direct dependencies between opam packages
  * use ocamlobjinfo to detect modules provided by libraries

*)

open EzCompat
open EzFile.OP
open Types

let main () =

  let opam_switch_prefix = try Sys.getenv "OPAM_SWITCH_PREFIX"
    with Not_found -> failwith "not in an opam switch"
  in


  let rec iter ~objinfo args =
    match args with
    | [] ->
        let state = Compute.compute ~opam_switch_prefix ~objinfo () in
        Printer.print state
    | "--no-objinfo" :: args ->
        iter ~objinfo:false args
    | ( "--help" | "-h" | "-help" ) :: _ ->
        Printf.eprintf "digodoc [--no-objinfo] [MODULE]\n%!";
        exit 0
    | _ :: _ :: _ ->
        Printf.eprintf "Error: digodoc takes 0 or 1 argument\n%!";
        exit 2
    | [ mdl ] ->
        let state = Compute.compute ~opam_switch_prefix ~objinfo () in
        match Hashtbl.find_all state.ocaml_mdls mdl with
        | exception Not_found -> failwith "module not found"
        | [ m ] ->
            if StringSet.mem "mli" m.mdl_exts then
              Unix.execvp "less" [| "less";
                                    opam_switch_prefix //
                                    ( Module.file m ".mli") |]
            else
            if StringSet.mem "ml" m.mdl_exts then
              Unix.execvp "less" [| "less";
                                    opam_switch_prefix //
                                    ( Module.file m "ml") |]
        | list ->
            List.iter (fun m ->
                Printf.printf "* %s::%s\n%!"
                  m.mdl_opam.opam_name m.mdl_name
              ) list;
            exit 0
  in

  let args = Sys.argv |> Array.to_list |> List.tl in
  iter ~objinfo:true args

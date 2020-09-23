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


open EzFile.OP
open Types

let main () =

  let opam_switch_prefix = try Sys.getenv "OPAM_SWITCH_PREFIX"
    with Not_found -> failwith "not in an opam switch"
  in

  let state = Compute.compute ~opam_switch_prefix in

  for i = 1 to Array.length Sys.argv - 1 do
    let m = Sys.argv.(i) in
    match Hashtbl.find_all state.ocaml_modules m with
    | exception Not_found -> failwith "module not found"
    | [ m ] ->
        if List.mem MLI m.mod_kinds then
          Unix.execvp "less" [| "less";
                                opam_switch_prefix //
                                ( m.mod_file ^ ".mli") |]
        else
        if List.mem ML m.mod_kinds then
          Unix.execvp "less" [| "less";
                                opam_switch_prefix //
                                ( m.mod_file ^ ".ml") |]
    | list ->
        List.iter (fun m ->
            Printf.printf "* %s::%s\n%!"
              m.mod_opam.opam_name m.mod_name
          ) list;
        exit 0

  done ;

  Printer.print state

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

open Types

let create ~mod_name ~mod_file ~mod_kind state ~mod_opam =
  let long_name = mod_opam.opam_name ^ "::" ^ mod_name in
  match Hashtbl.find state.ocaml_modules long_name with
  | exception Not_found ->

      let m = {
        mod_name ;
        mod_file ;
        mod_opam ;
        mod_kinds = [ mod_kind ];
      } in
      Hashtbl.add state.ocaml_modules long_name m ;
      Hashtbl.add state.ocaml_modules mod_name m ;
      m

  | m ->
      m.mod_kinds <- mod_kind :: m.mod_kinds ;
      m

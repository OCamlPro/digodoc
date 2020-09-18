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

open EzCompat
open Types

let create ?lib_dir state lib_opam ~lib_name ~lib_kind =
  let long_name = lib_opam.opam_name ^ "::" ^ lib_name in
  match Hashtbl.find state.ocaml_libraries long_name with
  | exception Not_found ->
      let lib = {
        lib_name ;
        lib_dir ;
        lib_metas = [];
        lib_modules = [];
        lib_interfaces = [];
        lib_kinds = [ lib_kind ] ;
        lib_opam ;
      } in
      Hashtbl.add state.ocaml_libraries long_name lib;
      Hashtbl.add state.ocaml_libraries lib_name lib;
      lib_opam.opam_libs <-
        StringMap.add lib_name lib lib_opam.opam_libs ;
      lib
  | lib ->
      if lib.lib_opam != lib_opam then
        Printf.kprintf failwith "Library %s defined by opam %S and %S"
          lib_name lib.lib_opam.opam_name lib_opam.opam_name;
      lib.lib_kinds <- lib_kind :: lib.lib_kinds;
      lib

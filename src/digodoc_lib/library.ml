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
open Type
open EzFile.OP

let long_name ~lib_opam ~lib_name =
  lib_opam.opam_name ^ "::" ^ lib_name

let file lib ~ext =
  lib.lib_dir.dir_name // ( lib.lib_name ^ ext )

let find state ~lib_opam ~lib_name =
  let long_name = long_name ~lib_opam ~lib_name in
  Hashtbl.find state.ocaml_libs_by_name long_name

let find_or_create ~lib_dir state lib_opam ~lib_name ~lib_ext ~objinfo =
  let lib =
    match find state ~lib_opam ~lib_name with
    | exception Not_found ->
        let lib = {
          lib_name ;
          lib_opam ;
          lib_dir ;

          lib_metas = StringMap.empty;
          lib_mdls = StringMap.empty;
          lib_exts = StringSet.empty;
          lib_units = [];
          lib_mld_files = [];
        } in
        Hashtbl.add state.ocaml_libs_by_name
          (long_name ~lib_opam ~lib_name) lib;
        Hashtbl.add state.ocaml_libs_by_name lib_name lib;
        lib_opam.opam_libs <-
          StringMap.add lib_name lib lib_opam.opam_libs ;
        state.ocaml_libs <-
          ( lib.lib_name ^ lib_opam.opam_name , lib ) :: state.ocaml_libs;
        lib_dir.dir_libs <- StringMap.add lib_name lib lib_dir.dir_libs ;

        if objinfo then
          lib.lib_units <- Objinfo.read state (file lib ~ext:".cmxa");

        lib
    | lib ->
        if lib.lib_opam != lib_opam then
          Printf.kprintf failwith "Library %s defined by opam %S and %S"
            lib_name lib.lib_opam.opam_name lib_opam.opam_name;
        lib
  in
  lib.lib_exts <- StringSet.add lib_ext lib.lib_exts;
  lib

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

type file_kind =
  | Directory
  | File
  | Link

type lib_kind = CMA | CMXA

type mod_kind = MLI | ML

type opam_package = {
  opam_name : string ;
  mutable opam_version : string option ;
  (* read .opam-switch/packages for that *)
  mutable opam_files : ( string * file_kind ) list ;
  (* meta files declared by this package *)
  mutable opam_metas : meta_package list ;
  (* libraries installed by this package *)
  mutable opam_libs : ocaml_library StringMap.t ;
  (*
  mutable opam_deps : opam_package list ;
  mutable opam_revdeps : opam_package list ;
*)
}

and meta_package = {
  meta_name : string ;
  meta_opam : opam_package ;
  meta_file : Meta_file.Types.t ;
  meta_parent : meta_package option ;
  mutable meta_dir : string ;
  mutable meta_subs : meta_package list ;
  (* which other metas are required by this meta *)
  mutable meta_deps : meta_package list ;
  mutable meta_revdeps : meta_package list ;
  (* which libraries are required by this meta *)
  mutable meta_libs : ocaml_library list ;
}

and ocaml_library = {
  lib_name : string ;
  lib_opam : opam_package ;
  mutable lib_dir : string option ;
  mutable lib_kinds : lib_kind list ;

  (* the list of metas that require directly this library *)
  mutable lib_metas : meta_package list;

  (* TODO: not filled for now *)
  mutable lib_modules : string list ;
  mutable lib_interfaces : string list;
}

and ocaml_module = {
  mod_name : string ;
  mod_file : string ;
  mod_opam : opam_package ;
  mutable mod_kinds : mod_kind list ;
}

and state = {
  opam_switch_prefix : string ;
  mutable opam_packages : opam_package StringMap.t ;
  mutable meta_packages : meta_package StringMap.t ;
  mutable ocaml_libraries : ( string, ocaml_library ) Hashtbl.t ;
  mutable ocaml_modules : ( string, ocaml_module ) Hashtbl.t ;
}

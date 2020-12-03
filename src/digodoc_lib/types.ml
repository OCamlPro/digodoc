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
  | Special

(* As returned by Objinfo.read : string -> unit list *)
type comp_unit = {
  unit_name : string ;
  unit_implementation : string option ; (* None for cmi *)
  unit_import_cmis : string StringMap.t ; (* Unit -> CMI CRC *)
  unit_import_cmxs : string StringMap.t ; (* Unit -> CMX CRC *)
}

type opam_package = {
  opam_name : string ;
  mutable opam_version : string ;
  (* read .opam-switch/packages for that *)
  mutable opam_files : ( string * file_kind ) list ;
  (* meta files declared by this package *)
  mutable opam_metas : meta_package list ;
  (* libraries installed by this package *)
  mutable opam_libs : ocaml_lib StringMap.t ;
  mutable opam_mdls : ocaml_mdl StringMap.t ;

  (* from the opam file *)
  mutable opam_synopsis : string option ;
  mutable opam_description : string option;
  mutable opam_authors : string list option;
  mutable opam_homepage : string option;
  mutable opam_license : string option;

  mutable opam_deps : opam_package StringMap.t ;
  mutable opam_revdeps : opam_package StringMap.t ;

  mutable opam_docs : doc_file list ;
}

and doc_file =
    README_md of string
  | CHANGES_md of string
  | LICENSE_md of string
  | ODOC_PAGE of string

and meta_package = {
  meta_name : string ;
  meta_opam : opam_package ;
  meta_file : Meta_file.Types.t ;
  meta_parent : meta_package option ;
  mutable meta_dir : directory ;
  mutable meta_subs : meta_package list ;
  (* which other metas are required by this meta *)
  mutable meta_deps : meta_package StringMap.t ;
  mutable meta_revdeps : meta_package StringMap.t ;

  (* which libraries are required by this meta : only .cmx and .cmxa *)
  mutable meta_archives : meta_archive StringMap.t ;
  mutable meta_libs : ocaml_lib StringMap.t ;
  mutable meta_mdls : ocaml_mdl StringMap.t ; (* individual cmx *)
}

and meta_archive =
  | Archive_lib of ocaml_lib
  | Archive_mdl of ocaml_mdl
  | Archive_missing

and directory = {
  dir_name : string ;
  mutable dir_meta : meta_package option ;
  mutable dir_libs : ocaml_lib StringMap.t ; (* NAME -> ocaml_lib *)
  mutable dir_mdls : ocaml_mdl StringMap.t ; (* NAME -> ocaml_mdl *)
}

and ocaml_lib = {
  lib_name : string ;
  lib_opam : opam_package ;
  lib_dir : directory ;
  mutable lib_exts : StringSet.t ;

  (* the list of metas that require directly this library *)
  mutable lib_metas : meta_package StringMap.t; (* NAME -> meta_package *)

  (* TODO: not filled for now *)
  mutable lib_mdls : ocaml_mdl StringMap.t ; (* NAME -> ocaml_mdl *)
  mutable lib_units : comp_unit list ;
}

and ocaml_mdl = {
  mdl_name : string ; (* Capitalized *)
  mdl_longname : string ;
  mdl_opam : opam_package ;
  mdl_dir : directory ;

  mutable mdl_basename : string ;
  mutable mdl_exts : StringSet.t ;
  mutable mdl_libs : ocaml_lib StringMap.t ; (* OPAM::NAME -> ocaml_lib *)

  (* meta_packages where this module appears EXPLICITELY *)
  mutable mdl_metas : meta_package StringMap.t; (* NAME -> meta_package *)

  mutable mdl_intf : comp_unit option ;
  mutable mdl_impl : comp_unit option ;
}

and state = {
  opam_switch_prefix : string ;
  mutable opam_packages : opam_package StringMap.t ;
  mutable meta_packages : meta_package StringMap.t ;

  mutable directories : directory StringMap.t ;
  (* both NAME -> ocaml_lib and OPAM::NAME -> ocaml_lib.
     Hashtbl because not injective *)
  mutable ocaml_libs : ( string * ocaml_lib ) list ;
  ocaml_libs_by_name : ( string, ocaml_lib ) Hashtbl.t ;

  (* both NAME -> ocaml_lib and OPAM::NAME -> ocaml_mdl.
     Hashtbl because not injective *)
  mutable ocaml_mdls : ( string * ocaml_mdl ) list ;
  ocaml_mdls_by_name : ( string, ocaml_mdl ) Hashtbl.t ;

  ocaml_mdls_by_cmi_crc : ( string, ocaml_mdl ) Hashtbl.t;
  ocaml_mdls_by_cmx_crc : ( string, ocaml_mdl ) Hashtbl.t;
}

(* Notes:
   * a module interface is public if the cmi is present
   * a module implementation is public if the [cmx+o/obj] is present
*)

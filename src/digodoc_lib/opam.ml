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

open EzFile.OP
open EzCompat
open OpamParserTypes
open Types


let create state opam_name opam_files =
  if StringMap.mem opam_name state.opam_packages then
    Printf.kprintf failwith "Duplicated opam package %s" opam_name;
  let p = {
    opam_name ;
    opam_version = None ;
    opam_files ;
    opam_metas = [] ;
    opam_libs = StringMap.empty ;
  } in
  state.opam_packages <- StringMap.add opam_name p state.opam_packages ;
  p

let kind = function
  | String _ -> "string"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Relop _ -> "relop"
  | Prefix_relop _ -> "prefix"
  | Logop _ -> "logop"
  | Pfxop _ -> "pfxop"
  | Ident _ -> "ident"
  | List _ -> "list"
  | Group _ -> "group"
  | Option _ -> "option"
  | Env_binding _ -> "env_binding"

let parse_changes filename =
  let opamfile = OpamParser.file filename in
  let files = ref [] in
  let add_file file info =
    let c = info.[0] in
    let kind =
      match c with
      | 'D' -> Directory
      | 'F' -> File
      | 'L' -> Link
      | _ ->
          Printf.eprintf "[ %c ]%!" c;
          assert false
    in
    files := (file, kind) :: !files
  in
  List.iter (function
      | Variable (_, "added", List (_, list) ) ->
          List.iter (function
              | Option (_, String (_, file),
                        [ String (_, info) ]) ->
                  add_file file info
              | _ -> assert false
            ) list
      | Variable (_, "added",
                  Option (_, String (_, file),
                          [ String (_, info) ])
                 ) ->
          add_file file info

      | Variable (_, name, v ) ->
          Printf.eprintf "Warning: unexpected %S of kind %s\n%!"
            name (kind v)
      | _ -> assert false) opamfile.file_contents;
  !files

let find_changes state =
  let changes_dir = state.opam_switch_prefix // ".opam-switch" // "install" in
  let files = try Sys.readdir changes_dir with _ -> [||] in
  let packages = ref [] in
  Array.iter (fun file ->
      match EzString.chop_suffix file ~suffix:".changes" with
      | None -> ()
      | Some name ->
          let changes = parse_changes ( changes_dir // file ) in
          packages := ( name, changes ) :: !packages
    ) files;
  !packages

let find_versions state =
  let versions_dir = state.opam_switch_prefix // ".opam-switch" // "packages" in
  let files = try Sys.readdir versions_dir with _ -> [||] in
  Array.iter (fun file ->
      let package, version = EzString.cut_at file '.' in
      match StringMap.find package state.opam_packages with
      | exception Not_found ->
          Printf.eprintf "Warning: opam package %S is installed \
                          without changes\n%!"
            package
      | opam_package ->
          opam_package.opam_version <- Some version
    ) files

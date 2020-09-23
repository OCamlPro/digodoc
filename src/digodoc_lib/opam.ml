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
    opam_synopsis = None ;
    opam_description = None;
    opam_authors = None ;
    opam_homepage = None ;
    opam_license = None ;
    opam_deps = StringMap.empty ;
    opam_revdeps = StringMap.empty ;
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

let iter_value_list v f =
  let rec iter_value v =
    match v with
    | String (_, name) -> f name [ String ( ("",0,0), "") ]
    | Option (_, String (_, name), option) -> f name option
    | Relop (_, _, v1, v2) ->
        iter_value v1 ; iter_value v2
    | Logop (_, _, v1, v2) -> iter_value v1 ; iter_value v2
    | Prefix_relop (_, _, v) -> iter_value v
    | Pfxop (_, _, v) -> iter_value v
    | Group (_, list) -> List.iter iter_value list
    | v
      ->
      Printf.eprintf "warning: unexpected depend value %s\n"
        ( OpamPrinter.value v);

  in
  match v with
  | List (_, values) ->
      List.iter iter_value values
  | v -> iter_value v

let find_versions state =
  let versions_dir = state.opam_switch_prefix // ".opam-switch" // "packages" in
  let files = try Sys.readdir versions_dir with _ -> [||] in
  Array.iter (fun file ->
      let package, version = EzString.cut_at file '.' in
      let filename = versions_dir // file // "opam" in
      match StringMap.find package state.opam_packages with
      | exception Not_found ->
          Printf.eprintf "Warning: opam package %S is installed \
                          without changes\n%!"
            package
      | opam_package ->
          opam_package.opam_version <- Some version ;
          match OpamParser.file filename with
          | exception _exn ->
              Printf.eprintf "Warning: cannot parse %s\n%!" filename
          | opam ->
              List.iter (function
                  | Variable (_, field, value) -> begin
                      match field, value with

                      | "synopsis", String (_, s) ->
                          opam_package.opam_synopsis <- Some s
                      | "description", String (_, s) ->
                          opam_package.opam_description <- Some s
                      | "homepage", String (_, s) ->
                          opam_package.opam_homepage <- Some s
                      | "license", String (_, s) ->
                          opam_package.opam_license <- Some s
                      | "authors", String (_, s) ->
                          opam_package.opam_authors <- Some [ s ]
                      | "authors", List (_, list) ->
                          opam_package.opam_authors <-
                            Some (List.map (function
                                | String (_,s) -> s
                                | _ -> "???") list)
                      | ( "name"
                        | "maintainer"
                        | "authors"
                        | "opam-version"
                        | "synopsis"
                        | "description"
                        | "homepage"
                        | "bug-reports"
                        | "license"
                        | "tags" (* ?? *)
                        | "dev-repo"
                        | "post-messages"
                        | "doc"
                        | "setenv"
                        | "conflict-class"
                        | "flags"
                        | "depexts"
                        | "version"
                        | "build"
                        | "install"
                        | "remove"
                        | "build-env"
                        | "conflicts"
                        | "substs"
                        | "extra-files") , _
                        -> ()
                      | ( "depends" | "depopts" ), v ->
                          iter_value_list v
                            (fun dep _constraints ->
                               match StringMap.find dep state.opam_packages with
                               | exception Not_found ->
                                   () (* probably optional *)
                               | opam_dep ->
                                   opam_package.opam_deps <-
                                     StringMap.add dep opam_dep
                                       opam_package.opam_deps;
                                   opam_dep.opam_revdeps <-
                                     StringMap.add package opam_package
                                       opam_dep.opam_revdeps;
                            )
                      | _ ->
                          Printf.eprintf "%s: discarding unknown field %S\n%!"
                            filename field;
                    end
                  | _ -> ()
                ) opam.file_contents
    ) files

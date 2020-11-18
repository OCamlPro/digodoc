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
open OpamParserTypes.FullPos
open Types


let create state opam_name opam_files =
  if StringMap.mem opam_name state.opam_packages then
    Printf.kprintf failwith "Duplicated opam package %s" opam_name;
  let p = {
    opam_name ;
    opam_version = "0.0" ;
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
    opam_mdls = StringMap.empty ;
    opam_docs = [] ;
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
  let opamfile = OpamParser.FullPos.file filename in
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
      | { pelem =
            Variable ( { pelem = "added" ; _ },
                       { pelem = List { pelem = list ; _ } ;  _ } );
          _ } ->
          List.iter (function
              | { pelem = Option (
                  { pelem = String file ; _ },
                  { pelem = [
                        { pelem = String info ; _ }
                      ]; _ });
                  _ } ->
                  add_file file info
              | _ -> assert false
            ) list
      | { pelem =
            Variable ( { pelem = "added" ; _ },
                       { pelem = Option
                             ( { pelem = String file ; _ },
                               { pelem = [
                                     { pelem = String info ; _ } ] ; _ } );
                         _
                       }
                     );
          _ } ->
          add_file file info

      | { pelem =
            Variable ( { pelem = name ; _ }, v ); _
        } ->
          Printf.eprintf "Warning: unexpected %S of kind %s\n%!"
            name (kind v.pelem)
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

let empty_pos = { filename = ""; start = 0,0; stop = 0,0 }
let elem pelem = { pelem ; pos = empty_pos }

let iter_value_list v f =
  let rec iter_value v =
    match v.pelem with
    | String name -> f name []
    | Option (
        { pelem = String name ; _ }, _option) -> f name []
    | Relop (_, v1, v2) ->
        iter_value v1 ; iter_value v2
    | Logop ( _, v1, v2) -> iter_value v1 ; iter_value v2
    | Prefix_relop (_, v) -> iter_value v
    | Pfxop (_, v) -> iter_value v
    | Group { pelem = list ; _ } -> List.iter iter_value list
    | _ ->
      Printf.eprintf "warning: unexpected depend value %s\n"
        ( OpamPrinter.FullPos.value v);

  in
  match v.pelem with
  | List { pelem = values ; _ } ->
      List.iter iter_value values
  | _ -> iter_value v

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
          opam_package.opam_version <- version ;
          match OpamParser.FullPos.file filename with
          | exception _exn ->
              Printf.eprintf "Warning: cannot parse %s\n%!" filename
          | opam ->
              List.iter (fun v ->
                  match v.pelem with
                  | Variable (field, value) -> begin
                      match field.pelem, value.pelem with

                      | "synopsis", String s ->
                          opam_package.opam_synopsis <- Some s
                      | "description", String s ->
                          opam_package.opam_description <- Some s
                      | "homepage", String s ->
                          opam_package.opam_homepage <- Some s
                      | "license", String s ->
                          opam_package.opam_license <- Some s
                      | "authors", String s ->
                          opam_package.opam_authors <- Some [ s ]
                      | "authors", List list ->
                          opam_package.opam_authors <-
                            Some (List.map (fun v ->
                                match v.pelem with
                                | String s -> s
                                | _ -> "???") list.pelem)
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
                        | "x-commit-hash"
                        | "patches"
                        | "run-test"
                        | "extra-files") , _
                        -> ()
                      | ( "depends" | "depopts" ), _ ->
                          iter_value_list value
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
                            filename field.pelem;
                    end
                  | _ -> ()
                ) opam.file_contents
    ) files

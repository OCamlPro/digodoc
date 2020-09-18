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


let check_file state opam_package file =

  let dirname = Filename.dirname file in
  let basename = Filename.basename file in
  if basename = "META" then
    let filename = state.opam_switch_prefix // file in
    let meta_name = Meta_file.Parser.name_of_META file in
(*    Printf.eprintf "opam package %S DEFINES ocamlfind package %S\n%!"
      opam_package.opam_name meta_name ; *)

    let meta_file = Meta_file.Parser.parse_file filename in
    let _m =
      Meta.create state ~meta_name ~meta_file
        ~meta_opam:opam_package ~meta_dir:dirname in
    ()
  else
    match EzString.chop_suffix basename ~suffix:".cma" with
    | Some lib_name ->
        let _lib =
          Library.create state opam_package
            ~lib_name ~lib_dir:dirname ~lib_kind:CMA
        in
        ()
    | None ->
        match EzString.chop_suffix basename ~suffix:".cmxa" with
        | Some lib_name ->
            let _lib =
              Library.create state opam_package
                ~lib_name ~lib_dir:dirname ~lib_kind:CMXA
            in
            ()
        | None ->
            match EzString.chop_suffix basename ~suffix:".mli" with
            | Some mod_name ->
                let mod_name = String.capitalize mod_name in
                let mod_file = Filename.chop_suffix file ".mli" in
                let _m =
                  Module.create state
                    ~mod_name ~mod_file ~mod_kind:MLI
                    ~mod_opam:opam_package
                in
                ()
            | None ->
                match EzString.chop_suffix basename ~suffix:".ml" with
                | Some mod_name ->
                    let mod_name = String.capitalize mod_name in
                    let mod_file = Filename.chop_suffix file ".ml" in
                    let _m =
                      Module.create state
                        ~mod_name ~mod_file ~mod_kind:ML
                        ~mod_opam:opam_package
                    in
                    ()
                | None ->
                    ()

let main () =

  let opam_switch_prefix = try Sys.getenv "OPAM_SWITCH_PREFIX"
    with Not_found -> failwith "not in an opam switch"
  in

  let state = {
    opam_switch_prefix ;
    opam_packages = StringMap.empty ;
    meta_packages = StringMap.empty ;
    ocaml_libraries = Hashtbl.create 13 ;
    ocaml_modules = Hashtbl.create 13 ;
  } in

  let packages = Opam.find_changes state in

  List.iter (fun (opam_name, opam_files) ->
      let opam_package = Opam.create state opam_name opam_files in
      List.iter (fun (file, _kind) ->
          check_file state opam_package file
        ) opam_files
    ) packages ;

  Opam.find_versions state ;

  StringMap.iter (fun _ meta_package ->

      Meta.find_requires state meta_package;

    ) state.meta_packages ;

  (* we need requires to be completely solved to correctly lookup libraries *)
  StringMap.iter (fun _ meta_package ->

      Meta.find_archives state meta_package

    ) state.meta_packages ;

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
              m.mod_name m.mod_opam.opam_name
          ) list;
        exit 0

  done ;

  Printer.print state

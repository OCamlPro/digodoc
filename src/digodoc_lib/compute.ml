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

  Warning:
  * Since META files from the OCaml distribution are wrongly installed
    by ocamlfind, we probably need to do something about it, no ?
   For example, the ones of compiler-libs.*

  TODO:
  * read doc/ files :
     README.md
     LICENSE.md
     CHANGES.md
     odoc-pages/index.mld

  * generate index pages for:
     META.$meta_package.$opam_package.$version/
     OPAM.$opam_package.$version/
  * Modules documentations are in:
     LIBRARY.$library.$opam_package.$version/
     MODULE.$mdl.$opam_package.$version/
*)


open EzCompat
open EzFile.OP
open Types

let check_file state ~objinfo opam_package file =

  let dirname = Filename.dirname file in
  let basename = Filename.basename file in
  let dir = Directory.get state dirname in
  if basename = "META" then
    let filename = state.opam_switch_prefix // file in
    let meta_name = Meta_file.Parser.name_of_META file in
    (*    Printf.eprintf "opam package %S DEFINES ocamlfind package %S\n%!"
          opam_package.opam_name meta_name ; *)

    let meta_file = Meta_file.Parser.parse_file filename in
    let _meta =
      Meta.create state ~meta_name ~meta_file
        ~meta_opam:opam_package ~meta_dir:dir in
    ()
  else
    (* TODO: register all interesting files, i.e. .cmxa, .cmx, .cmi,
       .cmti, .cmt, .mli, .ml *)
    let basename, ext = EzString.rcut_at basename '.' in
    match ext with
    | "cmxa" ->
        let ( _lib : ocaml_lib ) =
          Library.find_or_create state ~objinfo opam_package
            ~lib_name:basename ~lib_dir:dir ~lib_ext:ext
        in
        ()
    | "mli"
    | "ml"
    | "cmi"
    | "cmx"
    | "cmt"
    | "cmti" ->
        let ( _mdl : ocaml_mdl)  =
          Module.find_or_create state ~objinfo
            ~mdl_basename:basename ~mdl_ext:ext
            ~mdl_opam:opam_package ~mdl_dir:dir
        in
        ()

    | "mld" ->
        opam_package.opam_docs <- ODOC_PAGE file ::
                                  opam_package.opam_docs
    | "md" ->
        begin
          match basename with
          | "README" ->
              opam_package.opam_docs <-
                README_md file :: opam_package.opam_docs
          | "CHANGES" ->
              opam_package.opam_docs <-
                CHANGES_md file :: opam_package.opam_docs
          | "LICENSE" ->
              opam_package.opam_docs <-
                LICENSE_md file :: opam_package.opam_docs
          | _ -> ()
        end
    | _ -> ()

let find_modules state =
  StringMap.iter (fun _ opam ->
      StringMap.iter (fun _ lib ->
          List.iter (fun unit ->
              match StringMap.find unit.unit_name opam.opam_mdls with
              | exception Not_found ->
                  Printf.eprintf "%s::%s.%s has no external module\n%!"
                    opam.opam_name lib.lib_name unit.unit_name
              | mdl ->
                  lib.lib_mdls <- StringMap.add mdl.mdl_name mdl lib.lib_mdls;
                  mdl.mdl_libs <- StringMap.add lib.lib_name lib mdl.mdl_libs;
                  match mdl.mdl_impl with
                  | Some _ ->
                      () (* TODO: check it is the same one *)
                  | None ->
                      mdl.mdl_impl <- Some unit;
                      match unit.unit_implementation with
                      | None -> assert false
                      | Some crc ->
                          Hashtbl.add state.ocaml_mdls_by_cmx_crc crc mdl
            ) lib.lib_units
        ) opam.opam_libs
    ) state.opam_packages

let compute ~opam_switch_prefix ?(objinfo=false) () =

  let state = {
    opam_switch_prefix ;
    opam_packages = StringMap.empty ;
    meta_packages = StringMap.empty ;
    ocaml_libs_by_name = Hashtbl.create 13 ;
    ocaml_libs = [];
    ocaml_mdls = [] ;
    ocaml_mdls_by_name = Hashtbl.create 13 ;
    ocaml_mdls_by_cmi_crc = Hashtbl.create 13 ;
    ocaml_mdls_by_cmx_crc = Hashtbl.create 13 ;
    directories = StringMap.empty ;
  } in

  (* return a list of ( opam_package_name * changes) *)
  let packages = Opam.find_changes state in

  List.iter (fun (opam_name, opam_files) ->
      (* create the opam_package *)
      let opam_package = Opam.create state opam_name opam_files in

      (* create all existing meta_packages, ocaml_lib and ocaml_mdl,
         associated to their opam_packages and directories *)
      List.iter (fun (file, _kind) ->
          check_file state ~objinfo opam_package file
        ) opam_files;
      if opam_name = "ocaml-base-compiler" then
        check_file state ~objinfo opam_package "metas/META.stdlib";
    ) packages ;

  state.ocaml_libs <- List.sort compare state.ocaml_libs ;
  state.ocaml_mdls <- List.sort compare state.ocaml_mdls ;

  (* compute dependencies between opam_packages  *)
  Opam.find_versions state ;

  (* compute dependencies between meta_packages *)
  StringMap.iter (fun _ meta_package ->

      Meta.find_requires state meta_package;

    ) state.meta_packages ;

  (* Associate library modules to external modules *)
  if objinfo then find_modules state ;

  (* compute associations between meta_packages and
     ocaml_libs/ocaml_mdls. We need requires to be completely solved
     to correctly lookup libraries *)
  StringMap.iter (fun _ meta_package ->

      Meta.find_archives state meta_package

    ) state.meta_packages ;

  (* Try to associate "orphan" modules to libraries in the same package *)
  StringMap.iter (fun _n opam ->
      StringMap.iter (fun _n opam_mdl ->
          if StringMap.is_empty opam_mdl.mdl_libs &&
               StringSet.exists (fun s ->
                   s = "cmt" || s = "cmti" || s = "cmi"
                 ) opam_mdl.mdl_exts
          then
            StringMap.iter (fun _n lib ->
                if StringMap.exists (fun _n mdl ->
                       match mdl.mdl_intf with
                       | Some cu ->
                           StringMap.exists (fun un _crc ->
                               String.equal opam_mdl.mdl_name un
                             ) cu.unit_import_cmis ||
                             StringMap.exists (fun un _crc ->
                                 String.equal opam_mdl.mdl_name un
                               ) cu.unit_import_cmxs
                       | None ->
                           false
                     ) lib.lib_mdls
                then
                  begin
                    lib.lib_mdls <-
                      StringMap.add opam_mdl.mdl_name opam_mdl lib.lib_mdls;
                    opam_mdl.mdl_libs <-
                      StringMap.add lib.lib_name lib opam_mdl.mdl_libs
                  end
              ) opam.opam_libs
        ) opam.opam_mdls
    ) state.opam_packages;

  state

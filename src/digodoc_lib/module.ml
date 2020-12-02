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
open EzFile.OP

(* Assumption: all modules in the same directory are added by the
   same opam_file *)

let long_name ~mdl_name ~mdl_opam =
  mdl_opam.opam_name ^ "::" ^ mdl_name

let file mdl ~ext =
  mdl.mdl_dir.dir_name // ( mdl.mdl_basename ^ ext )

let find state ~mdl_name ~mdl_opam =
  let long_name = long_name ~mdl_name ~mdl_opam in
  Hashtbl.find state.ocaml_mdls_by_name long_name

let find_or_create ~mdl_ext ~mdl_dir ~mdl_basename state ~mdl_opam ~objinfo =
  let mdl_name = String.capitalize mdl_basename in
  let mdl =
    match find state ~mdl_name ~mdl_opam with
    | exception Not_found ->
        let mdl = {
          mdl_name ;
          mdl_longname = mdl_opam.opam_name ^ "::" ^ mdl_name;
          mdl_basename ;
          mdl_dir ;
          mdl_opam ;
          mdl_exts = StringSet.empty;
          mdl_libs = StringMap.empty;
          mdl_metas = StringMap.empty;
          mdl_intf = None;
          mdl_impl = None;
        } in
        Hashtbl.add state.ocaml_mdls_by_name
          ( long_name ~mdl_name ~mdl_opam ) mdl;
        Hashtbl.add state.ocaml_mdls_by_name mdl_name mdl ;
        state.ocaml_mdls <- ( mdl_name, mdl ) :: state.ocaml_mdls;
        mdl_dir.dir_mdls <- StringMap.add mdl.mdl_name mdl mdl_dir.dir_mdls;
        mdl_opam.opam_mdls <-
          StringMap.add mdl_name mdl mdl_opam.opam_mdls ;
        mdl
    | mdl -> mdl
  in
  mdl.mdl_exts <- StringSet.add mdl_ext mdl.mdl_exts ;
  (* TODO: check that, if this mdlule is already added, it is the same
     one. Otherwise, it means two opam packages have added different
     files for this mdlule *)
  if objinfo && mdl_ext = "cmx" then begin
    match Objinfo.read state (file mdl ~ext:".cmx") with
    | [] | _ :: _ :: _ -> (* TODO: warning *) ()
    | [ unit ] ->
        mdl.mdl_impl <- Some unit ;
        match unit.unit_implementation with
        | None -> assert false
        | Some crc ->
            Hashtbl.add state.ocaml_mdls_by_cmx_crc crc mdl
  end;
  if objinfo && mdl_ext = "cmi" then begin
    match Objinfo.read state (file mdl ~ext:".cmi") with
    | [] | _ :: _ :: _ -> (* TODO: warning *) ()
    | [ unit ] ->
        mdl.mdl_intf <- Some unit;
        match StringMap.find unit.unit_name unit.unit_import_cmis with
        | exception Not_found -> assert false
        | crc ->
            Hashtbl.add state.ocaml_mdls_by_cmi_crc crc mdl
  end;
  mdl

let file m ext =
  (* TODO: check that this extension exists for this mdlule *)
  Filename.concat m.mdl_dir.dir_name m.mdl_basename ^ "." ^ ext

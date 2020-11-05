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
open EzFile.OP
open Types

let pkg_of_mdl mdl = Printf.sprintf "%s@%s.%s"
    (match StringMap.bindings mdl.mdl_metas with
     | (_, meta) :: _rem -> meta.meta_name
     | [] ->
         match StringMap.bindings mdl.mdl_libs with
         | (_, lib) :: _rem -> lib.lib_name
         | [] ->
             (* TODO: some of these modules are interfaces without
                implementation. In such cases, we should attach them
                to the closest library that makes use of them in
                the dependency order. *)
             "MODULE_" ^ mdl.mdl_basename
    )
    mdl.mdl_opam.opam_name
    (match mdl.mdl_opam.opam_version with
     | None -> "VERSION"
     | Some s -> s)

let call_odoc state mdl ~dirs:_ ~pkgs ext =
  let pkg = pkg_of_mdl mdl in
  let odoc_target = "_digodoc" // "odoc" // pkg // mdl.mdl_basename ^ ".odoc" in
  let cmd = [
    "odoc" ; "compile" ;
    "--pkg" ; pkg ;
    "-o" ; odoc_target ;
  ]
(*    @
    (
      List.flatten @@
      List.map (fun dirname ->
          [ "-I" ; dirname ]
        ) dirs
    )
    @
    (
      List.flatten @@
      List.map (fun pkg ->
          [ "-I" ; dir_of_pkg pkg ]
        ) pkgs
      ) *)
    @
    [
      state.opam_switch_prefix // mdl.mdl_dir.dir_name //
      mdl.mdl_basename ^ ext ]
  in

  Process.call ( Array.of_list cmd );

  let cmd = [
    "odoc" ; "html" ;
    "-o" ; "_digodoc/html" ;
  ]
    @
(*    (
      List.flatten @@
      List.map (fun dirname ->
          [ "-I" ; dirname ]
        ) dirs
    )
      @ *)
    (
      List.flatten @@
      List.map (fun pkg ->
          [ "-I" ; "_digodoc" // "odoc" // pkg ]
        ) pkgs
    )
    @
    [ odoc_target ]
  in

  Process.call ( Array.of_list cmd );


  ()

let generate ~state =

  let done_table = Hashtbl.create 1111 in

  let rec check mdl =
    if not ( Hashtbl.mem done_table mdl.mdl_longname ) then begin
      Hashtbl.add done_table mdl.mdl_longname ();
      let dirs = ref StringSet.empty in
      let pkgs = ref StringSet.empty in
      begin
        match mdl.mdl_intf with
        | None -> ()
        | Some unit ->
            StringMap.iter (fun name crc ->
                match Hashtbl.find state.ocaml_mdls_by_cmi_crc crc with
                | exception Not_found ->
                    Printf.eprintf
                      "mdl %s depends on unfound %s with crc %s\n"
                      mdl.mdl_longname name crc
                | dep_mdl ->
                    check dep_mdl;
                    dirs := StringSet.add dep_mdl.mdl_dir.dir_name !dirs;
                    pkgs := StringSet.add (pkg_of_mdl dep_mdl) !pkgs;
              ) unit.unit_import_cmis
      end;
      let dirs = StringSet.to_list !dirs in
      let pkgs = StringSet.to_list !pkgs in
      Printf.eprintf "mdl %s checked\n" mdl.mdl_longname;
      if StringSet.mem "cmti" mdl.mdl_exts then
        call_odoc state mdl ~dirs ~pkgs ".cmti"
      else
      if StringSet.mem "cmi" mdl.mdl_exts then
        call_odoc state mdl ~dirs ~pkgs ".cmi"
    end
  in

  Hashtbl.iter (fun _ mdl ->
      check mdl
    ) state.ocaml_mdls_by_cmi_crc;

  ()

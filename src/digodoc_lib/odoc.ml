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

open Ez_html.V1
open EzCompat
open EzFile.OP
open Type

(* Note:
   In our first version, we were computing dependencies at the module level,
  but that's incompatible with `odoc`, because `odoc` follows "false"
  dependencies (module aliases), that do not appear at the module level
  (if it does not actually use them), causing `odoc` to fail with a missing
  dependency.
  TODO: we need to compute dependencies at the library level, i.e. compile
  documentation in the order of dependencies between LIBRARIES.

  (1) for every module, compile the `pkg` of the module
  (2) for every `pkg`, compute the dependencies of its modules to other `pkgs`
  (3) build the documentation in the topological order of the `pkgs`
*)

let digodoc_odoc_dir = Globals.digodoc_dir // "odoc"

let sources_dir = "sources"

let fullname opam = opam.opam_name ^ "." ^ opam.opam_version

let get_opam_sources ~continue_on_error opam =
  let package_name = fullname opam in
  let cmd = [
    "opam"; "source";
    "--dir"; sources_dir // package_name;
    package_name]
  in
    try
      Process.call ~continue_on_error (Array.of_list cmd)
    with exn ->
      Printf.eprintf "opam_error: %s\n%!" (Printexc.to_string exn)

let sources_of_opam opam =
  sources_dir // (fullname opam)

let htmlize_sources_of_opam opam =
  "_digodoc/sources" // fullname opam

let pkg_of_opam opam =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version

let pkg_of_pages opam file =
  Printf.sprintf "PAGES.%s.%s.%s"
    opam.opam_name opam.opam_version Filename.(dirname file |> dirname |> basename)

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s@%s.%s"
    lib.lib_name lib.lib_opam.opam_name lib.lib_opam.opam_version

let pkg_of_meta meta =
  Printf.sprintf "META.%s@%s.%s"
    meta.meta_name meta.meta_opam.opam_name meta.meta_opam.opam_version

let pkg_of_mdl mdl =
  let version = mdl.mdl_opam.opam_version in
  match StringMap.bindings mdl.mdl_libs with
  | (_, lib) :: _rem -> pkg_of_lib lib
  | [] ->
      let pack, alias = Index.module_cut mdl.mdl_basename in
      if alias = "" then
        Printf.sprintf "MODULE.%s@%s.%s"
          mdl.mdl_basename mdl.mdl_opam.opam_name version
      else
        let pkg =
          Printf.sprintf "MODULE.%s__@%s.%s"
            pack mdl.mdl_opam.opam_name version in
        if Sys.file_exists (digodoc_odoc_dir // pkg) then
          pkg
        else
          Printf.sprintf "MODULE.%s@%s.%s"
            pack mdl.mdl_opam.opam_name version

let mdl_is_alone mdl =
  StringMap.is_empty mdl.mdl_libs

let id_of_pkg pkg =
  snd (EzString.cut_at pkg '.')

let force_rebuild = match Sys.getenv "FORCE_REBUILD" with
  | exception Not_found -> false
  | _ -> true

let skip_modules = match Sys.getenv "SKIP_MODULES" with
  | exception Not_found -> false
  | _ -> true

let skip_packages = match Sys.getenv "SKIP_PACKAGES" with
  | exception Not_found -> false
  | _ -> true

let call_odoc ~continue_on_error state mdl ~pkgs ext =
  let pkg = pkg_of_mdl mdl in
  let odoc_target = digodoc_odoc_dir // pkg // mdl.mdl_basename ^ ".odoc" in
  let includes =
    List.flatten @@
    List.map (fun pkg ->
        [ "-I" ; digodoc_odoc_dir // pkg ]
      ) pkgs
  in
  if force_rebuild || not ( Sys.file_exists odoc_target ) then begin
    let cmd = [
      "odoc" ; "compile" ;
      "--pkg" ; pkg ;
      "-o" ; odoc_target ;
      state.opam_switch_prefix // mdl.mdl_dir.dir_name //
      mdl.mdl_basename ^ ext ]
      @ includes
    in
    try
      Process.call ~continue_on_error ( Array.of_list cmd );
      let cmd = [
        "odoc" ; "html" ;
        "--theme-uri"; "_odoc-theme" ;
        "-o" ; Html.digodoc_html_dir ;
        odoc_target ]
        @ includes
      in

      Process.call ~continue_on_error ( Array.of_list cmd );
    with exn ->
      Printf.eprintf "odoc_error: %s\n%!" (Printexc.to_string exn)
  end;
  ()

let call_odoc_compile ~continue_on_error state mdl ~pkgs ext =
  let pkg = pkg_of_mdl mdl in
  let odoc_target = digodoc_odoc_dir // pkg // mdl.mdl_basename ^ ".odoc" in
  let includes =
    List.flatten @@
    List.map (fun pkg ->
        [ "-I" ; digodoc_odoc_dir // pkg ]
      ) pkgs
  in
  if force_rebuild || not ( Sys.file_exists odoc_target ) then begin
    let cmd = [
      "odoc" ; "compile" ;
      "--pkg" ; pkg ;
      "-o" ; odoc_target ;
      state.opam_switch_prefix // mdl.mdl_dir.dir_name //
      mdl.mdl_basename ^ ext ]
      @ includes
    in
    try
      Process.call ~continue_on_error ( Array.of_list cmd )
    with exn ->
      Printf.eprintf "odoc_error: %s\n%!" (Printexc.to_string exn)
  end;
  ()

let call_odoc_html ~continue_on_error mdl ~pkgs =
  let pkg = pkg_of_mdl mdl in
  let html_target_dir = Html.digodoc_html_dir // pkg // mdl.mdl_name in
  let odoc_source = digodoc_odoc_dir // pkg // mdl.mdl_basename ^ ".odoc" in
  let includes =
    List.flatten @@
    List.map (fun pkg ->
        [ "-I" ; digodoc_odoc_dir // pkg ]
      ) pkgs
  in
  if force_rebuild || not ( Sys.file_exists html_target_dir ) then begin
    let cmd = [
      "odoc" ; "html" ;
      "--theme-uri"; "_odoc-theme" ;
      "-o" ; Html.digodoc_html_dir ;
      odoc_source ]
      @ includes
    in
    try
      Process.call ~continue_on_error ( Array.of_list cmd );
    with exn ->
      Printf.eprintf "odoc_error: %s\n%!" (Printexc.to_string exn)
  end;
  ()

let call_odoc_mld ~continue_on_error state pkg mldfile ~pkgs =
  let name = Filename.basename mldfile |> Filename.remove_extension in
  let odoc_target = digodoc_odoc_dir // pkg // "page-" ^ name ^ ".odoc" in
  let includes =
    List.concat_map (fun pkg ->
        [ "-I" ; digodoc_odoc_dir // pkg ]
      ) pkgs
  in
  if force_rebuild || not ( Sys.file_exists odoc_target ) then begin
    let cmd = [
      "odoc" ; "compile" ;
      "--pkg" ; pkg ;
      "-o" ; odoc_target ;
      state.opam_switch_prefix // mldfile ]
      @ includes
    in

    Process.call ~continue_on_error ( Array.of_list cmd );
  end;

  let cmd = [
    "odoc" ; "html" ;
    "--theme-uri"; "_odoc-theme" ;
    "-o" ; Html.digodoc_html_dir ;
    odoc_target ]
    @ includes
  in

  Process.call ~continue_on_error ( Array.of_list cmd );
  ()

let lookup_cmi state ~name ~crc =
  try Hashtbl.find state.ocaml_mdls_by_cmi_crc crc with
  | Not_found ->
      match Hashtbl.find_all state.ocaml_mdls_by_name name with
      | [ mdl ] -> mdl
      | _ ->
          raise Not_found

let opaque_crc = "--------------------------------"

let deps_of_pkg state =
  let deps_of_pkg = Hashtbl.create 1111 in

  List.iter (fun ( _, mdl ) ->

      let pkg = pkg_of_mdl mdl in

      let deps = try
          Hashtbl.find deps_of_pkg pkg
        with Not_found ->
          let deps = ref StringSet.empty in
          deps := StringSet.add pkg !deps;
          Hashtbl.add deps_of_pkg pkg deps;
          deps
      in

      begin
        match mdl.mdl_intf with
        | None -> ()
        | Some unit ->
            StringMap.iter (fun name crc ->
                match lookup_cmi state ~name ~crc with
                | exception Not_found ->
                    Printf.eprintf
                      "mdl %s depends on unfound cmi %s with crc %s\n"
                      mdl.mdl_longname name crc
                | dep_mdl ->
                    deps := StringSet.add (pkg_of_mdl dep_mdl) !deps
              ) unit.unit_import_cmis
      end;
      begin
        match mdl.mdl_impl with
        | None -> ()
        | Some unit ->
            StringMap.iter (fun name crc ->
                match lookup_cmi state ~name ~crc with
                | exception Not_found ->
                    Printf.eprintf
                      "mdl %s depends on unfound cmi %s with crc %s\n"
                      mdl.mdl_longname name crc
                | dep_mdl ->
                    deps := StringSet.add (pkg_of_mdl dep_mdl) !deps;
              ) unit.unit_import_cmis;
            StringMap.iter (fun name crc ->
                match Hashtbl.find state.ocaml_mdls_by_cmx_crc crc with
                | exception Not_found ->
                    Printf.eprintf
                      "mdl %s depends on unfound cmx %s with crc %s\n"
                      mdl.mdl_longname name crc
                | dep_mdl ->
                    deps := StringSet.add (pkg_of_mdl dep_mdl) !deps;
              ) unit.unit_import_cmxs;
      end;

    ) state.ocaml_mdls ;

  deps_of_pkg

let iter_modules_with_cmi state f =

  let done_table = Hashtbl.create 1111 in

  let rec check stack mdl =
    if not ( Hashtbl.mem done_table mdl.mdl_longname ) then begin
      Hashtbl.add done_table mdl.mdl_longname ();
      let stack = mdl.mdl_longname :: stack in
      begin
        match mdl.mdl_intf with
        | None -> ()
        | Some unit ->
            StringMap.iter (fun name crc ->
                if crc <> opaque_crc then
                  match lookup_cmi state ~name ~crc with
                  | exception Not_found ->
                      Printf.eprintf
                        "mdl %s depends on unfound cmi %s with crc %s\n"
                        mdl.mdl_longname name crc
                  | dep_mdl ->
                      if dep_mdl != mdl then
                        check stack dep_mdl
              ) unit.unit_import_cmis
      end;
      begin
        match mdl.mdl_impl with
        | None -> ()
        | Some unit ->
            StringMap.iter (fun name crc ->
                if crc <> opaque_crc then
                  match lookup_cmi state ~name ~crc with
                  | exception Not_found ->
                      Printf.eprintf
                        "mdl %s depends on unfound cmi %s with crc %s\n"
                        mdl.mdl_longname name crc
                  | dep_mdl ->
                      if dep_mdl != mdl then
                        check stack dep_mdl
              ) unit.unit_import_cmis;
            StringMap.iter (fun name crc ->
                match Hashtbl.find state.ocaml_mdls_by_cmx_crc crc with
                | exception Not_found ->
                    Printf.eprintf
                      "mdl %s depends on unfound cmx %s with crc %s\n"
                      mdl.mdl_longname name crc
                | dep_mdl ->
                    if dep_mdl != mdl then
                      check stack dep_mdl
              ) unit.unit_import_cmxs;
      end;
      f state mdl
    end else
    if List.mem mdl.mdl_longname stack then begin
      Printf.eprintf "Cycle:\n";
      List.iter (fun name ->
          Printf.eprintf "    %s <- \n%!" name
        ) ( mdl.mdl_longname :: stack)
    end
  in
  Hashtbl.iter (fun _ mdl ->
      check [] mdl
    ) state.ocaml_mdls_by_cmi_crc


type modul = {
  modul : ocaml_mdl ;
  mutable modul_subs : ocaml_mdl StringMap.t ;
}

let insert_html f =
  let b = Buffer.create 1000 in
  f b;
  let html = Buffer.contents b in
  Printf.sprintf "{%%html:%s%%}\n" (HTML.check html)

let modules_to_html map =
  match StringMap.bindings map with
  | [] -> ""
  | (_, _mdl0) :: _ ->

      insert_html (fun b ->
          let new_map = ref StringMap.empty in
          StringMap.iter (fun _ mdl ->
              match mdl.mdl_intf with
              | None -> ()
              | Some _ ->
                  let m, sub = Index.module_cut mdl.mdl_name in
                  match StringMap.find m !new_map with
                  | exception Not_found ->
                      new_map := StringMap.add mdl.mdl_name {
                          modul = mdl ;
                          modul_subs = StringMap.empty ;
                        } !new_map
                  | modul ->
                      if sub <> "" then
                        modul.modul_subs <- StringMap.add sub mdl modul.modul_subs
            ) map ;

          Printf.bprintf b {|<ul class="modules">|};
          StringMap.iter (fun _ modul ->
              let mdl = modul.modul in
              let pkg = pkg_of_mdl mdl in
              Printf.bprintf b
                {|<li><a href="../%s/%s/index.html">%s</a> %s</li>|}
                pkg mdl.mdl_name mdl.mdl_name
                (if StringMap.is_empty modul.modul_subs then
                   ""
                 else
                   let pack_path =
                     digodoc_odoc_dir // pkg //
                       mdl.mdl_basename ^ "__.odoc" in
                   let pack =
                     if Sys.file_exists pack_path then
                       mdl.mdl_name ^ "__"
                     else
                       mdl.mdl_name
                   in
                   Printf.sprintf " .{ %s }"
                     ( String.concat ", "
                         ( List.map (fun (name, mdl) ->
                               let pkg = pkg_of_mdl mdl in
                               Printf.sprintf
                                 {|<a href="../%s/%s/%s/index.html">%s</a>|}
                                 pkg pack name name
                             )
                               ( StringMap.bindings modul.modul_subs )))
                )
            ) !new_map;
          Printf.bprintf b {|</ul>|};
        )


let print_package_info b lines =
  Printf.bprintf b {|{%%html:<table class="package info">|};
  List.iter (fun line ->
      Printf.bprintf b {|<tr>|};
      List.iter (fun col ->
          Printf.bprintf b {|<td>%s</td>|} col
        ) line;
      Printf.bprintf b {|</tr>|};
    ) lines;
  Printf.bprintf b {|</table>%%}|};
  ()

let opams_to_html title map =
  match StringMap.bindings map with
  | [] -> [ title ^ "(0)" ; ""]
  | list -> [
      Printf.sprintf "%s (%d)" title ( List.length list );
      Printf.sprintf "<ul>%s</ul>"
        ( String.concat ""
            ( List.map (fun (_,opam) ->
                  let pkg = pkg_of_opam opam in
                  let nv = opam.opam_name ^ "." ^ opam.opam_version in
                  Printf.sprintf
                    {|<li><a href="../%s/index.html">%s</a></li>|}
                    pkg nv
                ) list))
    ]

let metas_to_html title map =
  match StringMap.bindings map with
  | [] -> [ title ^ "(0)" ; ""]
  | list -> [
      Printf.sprintf "%s (%d)" title ( List.length list );
      Printf.sprintf "<ul>%s</ul>"
        ( String.concat ""
            ( List.map (fun (_,meta) ->
                  let pkg = pkg_of_meta meta in
                  Printf.sprintf
                    {|<li><a href="../%s/index.html">%s</a></li>|}
                    pkg meta.meta_name
                ) list))
    ]

let metas_list_to_html title list =
  match list with
  | [] -> [ title ^ "(0)" ; ""]
  | list -> [
      Printf.sprintf "%s (%d)" title ( List.length list );
      Printf.sprintf "<ul>%s</ul>"
        ( String.concat ""
            ( List.map (fun meta ->
                  let pkg = pkg_of_meta meta in
                  Printf.sprintf
                    {|<li><a href="../%s/index.html">%s</a></li>|}
                    pkg meta.meta_name
                ) list))
    ]

let libraries_to_html title map =
  match StringMap.bindings map with
  | [] -> [ title ^ "(0)" ; ""]
  | list -> [
      Printf.sprintf "%s (%d)" title ( List.length list );
      Printf.sprintf "<ul>%s</ul>"
        ( String.concat ""
            ( List.map (fun (name,lib) ->
                  let pkg = pkg_of_lib lib in
                  Printf.sprintf
                    {|<li><a href="../%s/index.html">%s</a></li>|}
                    pkg name
                ) list))
    ]




let infos_of_opam state pkg opam =
  let html_dir = Html.digodoc_html_dir // pkg in

  let omd_generate_file file =
    let basename = Filename.basename file in
    let name,_ = EzFile.cut_extension basename in
    let html_file = (pkg // name ^ ".html") in
    let srcfile = ( state.opam_switch_prefix // file ) in
    if Sys.file_exists srcfile then begin
      EzFile.make_dir ~p:true html_dir ;
      let generate bb ~title =
        ignore title;
        let content = try EzFile.read_file srcfile |> Omd.of_string |> Omd.to_html with _ -> "" in
        Printf.bprintf bb {|%s|} content 
      in 
      Html.generate_page
        ~filename:html_file
        ~title:"basename"
        generate ;
      Printf.sprintf {|<a href="%s.html">%s</a>|} name basename
    end
    else begin
      Printf.eprintf "odoc_error: missing file %s\n%!" srcfile;
      ""
    end
  in

  let opam_pkg = pkg_of_opam opam in
  [
    [ "opam-name" ;
      Printf.sprintf {|<a href="../%s/index.html">%s</a>|}
        opam_pkg opam.opam_name ];
    [ "opam-version" ; opam.opam_version ] ;
  ] @
  ( match opam.opam_synopsis with
    | None -> []
    | Some synopsis ->
        [  [ "synopsis" ; Html.encode synopsis ] ]
  ) @
  ( match opam.opam_description with
    | None -> []
    | Some s ->
        [  [ "description" ; Html.encode s ] ]
  ) @
  ( match opam.opam_authors with
    | None -> []
    | Some authors ->
        [  [ "authors" ;
             Printf.sprintf "<ul>%s</ul>"
               (String.concat ""
                  (List.map (fun s ->
                       Printf.sprintf "<li>%s</li>"
                         ( HTML.encode s )) authors))
           ] ]
  ) @
  ( match opam.opam_homepage with
    | None -> []
    | Some homepage ->
        [  [ "homepage" ;
             Printf.sprintf {|<a href="%s">%s</a>|}
               homepage homepage ] ]
  ) @
  ( match opam.opam_license with
    | None -> []
    | Some license ->
        [  [ "license" ; license ] ]
  ) @
  ( match opam.opam_source_archive with
    | None -> []
    | Some archive ->
        [  [ "source-archive" ; 
              Printf.sprintf {|<a href="%s">%s</a>|}
               archive archive ] ]
  ) @
  (
    List.map (function
        | README_md file -> [ "readme-file" ; omd_generate_file file ]
        | CHANGES_md file -> [ "changes-file" ; omd_generate_file file ]
        | LICENSE_md file -> [ "license-file" ; omd_generate_file file ]
        | ODOC_PAGE file -> [ "odoc-file" ; file ]
      ) opam.opam_docs
  )


let save_line ~dir_name ~line_name line =

  let mdl_dir = Html.digodoc_html_dir // dir_name in
  EzFile.make_dir ~p:true mdl_dir;
  EzFile.write_file ( mdl_dir // line_name )
    line;
  ()


let generate_library_pages state =

  List.iter (fun  ( _, lib ) ->
      let pkg = pkg_of_lib lib in
      let opam_pkg = pkg_of_opam lib.lib_opam in

      Index.SAVE.save_library_entry
        ( Html.digodoc_html_dir // pkg // "ENTRY.LIBRARY." ^ lib.lib_name )
        lib;

      if not skip_packages then
        let b = Buffer.create 10000 in

        begin match
            List.find_opt
              (fun mld -> Filename.basename mld = "index.mld")
              lib.lib_mld_files
          with
          | None ->
              Printf.bprintf b "{0:library-%s Library %s\n"
                lib.lib_name lib.lib_name ;
              Printf.bprintf b
                {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
                opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
              Printf.bprintf b "}\n";
          | Some mld ->
              let in_chan = open_in @@ state.opam_switch_prefix // mld in
              let in_buffer = Bytes.create 1024 in
              let rec loop () =
                let l = input in_chan in_buffer 0 1024 in
                if l <> 0
                then ( Buffer.add_subbytes b in_buffer 0 l; loop () )
                else ()
              in loop ();
              Printf.bprintf b
                {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
                opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
        end ;

        Printf.bprintf b "{1:info Library info}\n";
        Printf.bprintf b {|{%%html:<table class="package info">|};
        Printf.bprintf b {|<tr><td>Opam package:</td><td><a href="../%s/index.html" class="digodoc-opam">%s.%s</a></td></tr>|}
          opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
        Printf.bprintf b {|<tr><td>Directory:</td><td>%s</td></tr>|}
          lib.lib_dir.dir_name;
        Printf.bprintf b {|<tr><td>Dune/OCamlfind:</td><td>%s</td></tr>|}
          (String.concat " "
             (StringMap.bindings lib.lib_metas |>
              List.map (fun (_, meta) ->
                  Printf.sprintf {|<a href="../%s/index.html">%s</a>|}
                    (pkg_of_meta meta)
                    meta.meta_name
                )
             ));
        Printf.bprintf b "</table>%%}\n";

        Printf.bprintf b "{1:modules Library modules}\n";
        Printf.bprintf b "%s\n"
          (modules_to_html lib.lib_mdls);

        let content = Buffer.contents b in
        let mld_file = digodoc_odoc_dir // pkg // "page-index.mld" in

        let dirname = Filename.dirname mld_file in
        EzFile.make_dir ~p:true dirname ;
        EzFile.write_file mld_file content;

        let odoc_target = digodoc_odoc_dir // pkg //  "page-index.odoc" in
        let cmd = [
          "odoc" ; "compile" ;
          "--pkg" ; pkg ;
          "-o" ; odoc_target ;
          mld_file
        ]
        in
        Process.call ( Array.of_list cmd );

        let cmd = [
          "odoc" ; "html" ;
          "--theme-uri"; "_odoc-theme" ;
          "-o" ; Html.digodoc_html_dir ;
          "-I" ; digodoc_odoc_dir // pkg ;
          odoc_target
        ]
        in

        Process.call ( Array.of_list cmd );

        ()
    )
    ( List.sort compare state.ocaml_libs );

  ()

let recursive_deps_memo : opam_package StringMap.t StringMap.t ref = ref StringMap.empty

let get_recursive_deps opam = (* TODO: fix cycles due to post *)
  let rec aux grey opam =
    if StringSet.mem opam.opam_name grey
    then StringMap.empty
    else begin
      let grey = StringSet.add opam.opam_name grey in
      match StringMap.find_opt opam.opam_name !recursive_deps_memo with
      | None ->
          Printf.printf "descending into %s\n%!" opam.opam_name;
          let result =
            StringMap.fold (fun n o res ->
                if StringMap.mem n res
                then res
                else
                  let rdeps = aux grey o in
                  StringMap.union (fun _ a _ -> Some a) res rdeps)
              opam.opam_deps StringMap.empty
          in
          recursive_deps_memo := StringMap.add opam.opam_name result !recursive_deps_memo;
          result
      | Some res -> res
    end
  in aux StringSet.empty opam

let generate_opam_pages ~continue_on_error state =

  StringMap.iter (fun _ opam ->
      let pkg = pkg_of_opam opam in

      Index.SAVE.save_opam_entry
        ( Html.digodoc_html_dir // pkg // "ENTRY.OPAM." ^ opam.opam_name )
        opam ;

      if not skip_packages then
        let b = Buffer.create 10000 in

        let get_rec_deps = ref false in
        begin match
            List.find_map
              (function
                | ODOC_PAGE mld when Filename.basename mld = "index.mld" ->
                    if Filename.( mld |> dirname |> dirname |> basename ) = opam.opam_name
                    then Some mld
                    else None
                | README_md _ | CHANGES_md _ | LICENSE_md _ | ODOC_PAGE _ -> None )
              opam.opam_docs
          with
          | None ->
              Printf.bprintf b "{0:opam-%s.%s Opam Package %s.%s\n"
                opam.opam_name opam.opam_version
                opam.opam_name opam.opam_version;
      (*
      Printf.bprintf b
        {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
        opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
*)
              Printf.bprintf b "}\n";

          | Some mld ->
              get_rec_deps := true;
              let in_chan = open_in @@ state.opam_switch_prefix // mld in
              let in_buffer = Bytes.create 1024 in
              let rec loop () =
                let l = input in_chan in_buffer 0 1024 in
                if l <> 0
                then ( Buffer.add_subbytes b in_buffer 0 l; loop () )
                else ()
              in loop ();
              (*Printf.bprintf b
                {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
                opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;*)
        end ;

        let pages_pkgs = ref StringSet.empty in
        let mldfiles = List.filter_map (function
            | README_md _ | CHANGES_md _ | LICENSE_md _ -> None
            | ODOC_PAGE mld -> Some mld
          ) opam.opam_docs in
        if mldfiles <> []
        then begin
          Printf.bprintf b "{1:pages Package documentation pages}\n";
          Printf.bprintf b "{%%html:\n";
          Printf.bprintf b {|<ul class="pages">|};
          List.iter (fun mld ->
              let ppkg = pkg_of_pages opam mld in
              pages_pkgs := StringSet.add ppkg !pages_pkgs;
              let odeps = get_recursive_deps opam in
              call_odoc_mld ~continue_on_error state ppkg mld
                ~pkgs:( ppkg :: pkg :: []
                        |> StringMap.fold (fun _ o acc ->
                            StringMap.fold (fun _ l acc -> pkg_of_lib l :: acc)
                              o.opam_libs acc) odeps
                      );
              let name = Filename.(chop_extension @@ basename mld) in
              Printf.bprintf b {|<li><a href="../%s/%s.html">%s</a></li>|}
                ppkg name (String.capitalize_ascii name);
            ) mldfiles;
          Printf.bprintf b "</ul>\n";
          Printf.bprintf b "%%}\n";
          (* TODO, generate the doc and put in a link *)
        end;

        Printf.bprintf b "\n{1:modules Documentation on Modules}\n";

        Printf.bprintf b "%s\n"
          (modules_to_html opam.opam_mdls);

        Printf.bprintf b "{1:info Package info}\n";

        let dir = digodoc_odoc_dir // pkg in
        EzFile.make_dir ~p:true dir;

        let infos = infos_of_opam state pkg opam in
        let infos =
          infos @ [
            opams_to_html "deps" opam.opam_deps ;
            opams_to_html "revdeps" opam.opam_revdeps ;
            metas_list_to_html "metas" opam.opam_metas ;
            libraries_to_html "libraries" opam.opam_libs ;
          ]
        in
        print_package_info b infos;

        if !Htmlize.Globals.sources then begin 

          Printf.bprintf b "\n{1:sources Package sources}\n";

          let opam_sources = htmlize_sources_of_opam opam in
          Printf.bprintf b {|{%%html:<div><a href="../../%s/index.html">%s</a></div>%%}|}
            opam_sources opam.opam_name
        end;

        Printf.bprintf b "\n{1:files Package files}\n";
        Printf.bprintf b {|{%%html:<pre>|};
        List.iter (fun (file, _) ->
            Printf.bprintf b "%s\n" file
          ) opam.opam_files;
        Printf.bprintf b {|</pre>%%}|};

        let content = Buffer.contents b in
        let mld_file = dir // "page-index.mld" in

        EzFile.write_file mld_file content;

        let odoc_target = digodoc_odoc_dir // pkg //  "page-index.odoc" in
        let cmd = [
          "odoc" ; "compile" ;
          "--pkg" ; pkg ;
          "-o" ; odoc_target ;
          mld_file
        ]
        in
        Process.call ( Array.of_list cmd );

        let cmd = [
          "odoc" ; "html" ;
          "--theme-uri"; "_odoc-theme" ;
          "-o" ; Html.digodoc_html_dir ;
          "-I" ; digodoc_odoc_dir // pkg ;
          odoc_target
        ] @
          List.flatten (List.map (fun (_,lib) ->
              [ "-I" ; digodoc_odoc_dir // pkg_of_lib lib ]
            ) (StringMap.to_list opam.opam_libs)) @
          (if !get_rec_deps then
             List.concat_map (fun (_,opam) ->
                 List.concat_map (fun (_,lib) ->
                     ["-I"; digodoc_odoc_dir // pkg_of_lib lib]
                   ) (StringMap.to_list opam.opam_libs))
               (StringMap.to_list (get_recursive_deps opam))
           else [] ) @
          List.concat_map (fun pkg -> ["-I"; digodoc_odoc_dir // pkg])
            (StringSet.to_list !pages_pkgs)
        in

        Process.call ( Array.of_list cmd );

        ()
    ) state.opam_packages ;

  ()

let generate_module_entries state =

  List.iter (fun ( _ , mdl ) ->
      let pkg = pkg_of_mdl mdl in
      if StringSet.mem "cmi" mdl.mdl_exts then
      Index.SAVE.save_module_entry
        ( Html.digodoc_html_dir // pkg // "ENTRY.MODULE." ^ mdl.mdl_name )
        mdl ;
    ) state.ocaml_mdls ;

  ()

let generate_meta_pages state =


  StringMap.iter (fun _ meta ->
      let pkg = pkg_of_meta meta in
      let opam = meta.meta_opam in
      let opam_pkg = pkg_of_opam opam in

      Index.SAVE.save_meta_entry
        ( Html.digodoc_html_dir // pkg // "ENTRY.META." ^ meta.meta_name )
        meta;

      if not skip_packages then
        let b = Buffer.create 10000 in

        Printf.bprintf b "{0:opam-%s Meta Package %s\n"
          meta.meta_name meta.meta_name;
        Printf.bprintf b
          {|{%%html:<nav><a href="../%s/index.html" class="digodoc-opam">%s.%s</a></nav>%%}|}
          opam_pkg opam.opam_name opam.opam_version;
        Printf.bprintf b "}\n";
        Printf.bprintf b
          "Meta packages are used by ocamlfind and in the libraries statement of dune.\n";

        Printf.bprintf b "{1:info Package info}\n";

        let infos = infos_of_opam state pkg meta.meta_opam in
        let infos =
          infos @ [
            metas_to_html "deps" meta.meta_deps ;
            metas_to_html "revdeps" meta.meta_revdeps ;
          ]
        in
      (*
      let infos =
        infos @ [
          opams_to_html "deps" opam.opam_deps ;
          opams_to_html "revdeps" opam.opam_revdeps ;
          metas_list_to_html "metas" opam.opam_metas ;
          libraries_to_html "libraries" opam.opam_libs ;
        ]
      in
*)
        print_package_info b infos;

(*
      Printf.bprintf b "{1:info Library info}\n";
      Printf.bprintf b {|{%%html:<table class="package info">|};
      Printf.bprintf b {|<tr><td>Opam package:</td><td><a href="../%s/index.html">%s.%s</a></td></tr>|}
        opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
      Printf.bprintf b {|<tr><td>Directory:</td><td>%s</td></tr>|}
        lib.lib_dir.dir_name;
      Printf.bprintf b {|<tr><td>Dune/OCamlfind:</td><td>%s</td></tr>|}
        (String.concat " "
           (StringMap.bindings lib.lib_metas |>
            List.map (fun (_, meta) ->
                Printf.sprintf {|<a href="%s/index.html">%s</a>|}
                  (pkg_of_meta meta)
                  meta.meta_name
              )
           ));
      Printf.bprintf b {|</table>%%}|};
*)

        Printf.bprintf b "\n{1:modules Package modules}\n";

        let map = ref meta.meta_mdls in
        StringMap.iter (fun _ lib ->
            map := StringMap.union (fun _ a _ -> Some a) lib.lib_mdls !map
          ) meta.meta_libs;

        Printf.bprintf b "%s\n" (modules_to_html !map);

        let content = Buffer.contents b in
        let mld_file = digodoc_odoc_dir // pkg // "page-index.mld" in

        let dirname = Filename.dirname mld_file in
        EzFile.make_dir ~p:true dirname ;
        EzFile.write_file mld_file content;

        let odoc_target = digodoc_odoc_dir // pkg //  "page-index.odoc" in
        let cmd = [
          "odoc" ; "compile" ;
          "--pkg" ; pkg ;
          "-o" ; odoc_target ;
          mld_file
        ]
        in
        Process.call ( Array.of_list cmd );

        let cmd = [
          "odoc" ; "html" ;
          "--theme-uri"; "_odoc-theme" ;
          "-o" ; Html.digodoc_html_dir ;
          "-I" ; digodoc_odoc_dir // pkg ;
          odoc_target
        ]
        in

        Process.call ( Array.of_list cmd );

        ()
    ) state.meta_packages ;

  ()

let generate ~state ~continue_on_error  =
  (* Iter on modules first *)
  if Sys.file_exists Html.digodoc_html_dir then begin
    EzFile.remove_dir ~all:true Html.digodoc_html_dir
  end;
  EzFile.make_dir ~p:true Html.digodoc_html_dir;
  Process.call [|
    "rsync"; "-auv"; "html/.";  Html.digodoc_html_dir // "." |];

  let deps_of_pkg = deps_of_pkg state in

  if force_rebuild || not skip_modules then begin

    iter_modules_with_cmi state (fun state mdl ->
        let pkg = pkg_of_mdl mdl in
        let pkgs = Hashtbl.find deps_of_pkg pkg in
        let pkgs = StringSet.to_list !pkgs in
        if StringSet.mem "cmti" mdl.mdl_exts then
          call_odoc_compile ~continue_on_error state mdl ~pkgs ".cmti"
        else
        if StringSet.mem "cmt" mdl.mdl_exts then
          call_odoc_compile ~continue_on_error state mdl ~pkgs ".cmt"
        else
        if StringSet.mem "cmi" mdl.mdl_exts then
          call_odoc_compile ~continue_on_error state mdl ~pkgs ".cmi"
      );

    iter_modules_with_cmi state (fun _state mdl ->
        let pkg = pkg_of_mdl mdl in
        let pkgs = Hashtbl.find deps_of_pkg pkg in
        let pkgs = StringSet.to_list !pkgs in
        if not (StringSet.disjoint mdl.mdl_exts
                  (StringSet.of_list ["cmti";"cmt";"cmi"])) then
          call_odoc_html ~continue_on_error mdl ~pkgs
      )
  end;

  if !Htmlize.Globals.sources then begin
    Htmlize.Globals.with_header := true;
    EzFile.make_dir ~p:true sources_dir;
    StringMap.iter (fun _ opam ->
      let opam_sources = sources_of_opam opam
      and opam_htmlize_sources = htmlize_sources_of_opam opam in
      if not (Sys.file_exists opam_htmlize_sources) then begin
        get_opam_sources ~continue_on_error opam;
        if not (Sys.file_exists Globals.htmlize_sources_dir) then
          Htmlize.Main.htmlize Globals.htmlize_sources_dir [opam_sources]
        else
          Htmlize.Main.htmlize_dir Globals.htmlize_sources_dir opam_sources;
        EzFile.remove_dir ~all:true opam_sources
      end
    ) state.opam_packages;
    EzFile.remove_dir ~all:true sources_dir
  end;

  generate_opam_pages ~continue_on_error state;
  generate_library_pages state;
  generate_meta_pages state;
  generate_module_entries state;

  ()

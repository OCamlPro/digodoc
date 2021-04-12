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

module TYPES = struct

  type opam_entry = {
    opam_name : string ;
    opam_version : string ;
    opam_synopsis : string ;
  }

  type meta_entry = {
    meta_name : string ;
    meta_opam_name : string ;
    meta_opam_version : string ;
  }

  type library_entry = {
    lib_name : string ;
    lib_opam_name : string ;
    lib_opam_version : string ;
  }

  type module_entry = {
    mdl_name : string ; (* file/lowercase *)
    mdl_opam_name : string ;
    mdl_opam_version : string ;
    mdl_basename : string ;
    mdl_libs : library_entry list ;
  }

  type source_entry = {
    src_opam_name: string;
    src_opam_version: string;
  }

  type entry =
      Module of module_entry
    | Library of library_entry
    | Opam of opam_entry
    | Meta of meta_entry
    | Source of source_entry

end

module SAVE = struct

  let open_out filename =
    EzFile.make_dir ~p:true ( Filename.dirname filename );
    open_out filename

  open Type

  let save_opam_entry file opam =
    let oc = open_out file in
    Printf.fprintf oc "opam\n";
    Printf.fprintf oc "%s\n" opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim opam.opam_version);
    Printf.fprintf oc "%s\n" (match opam.opam_synopsis with
        | None -> "" | Some s ->
            String.trim (String.concat " " (EzString.split s '\n'))
      );
    close_out oc

  let save_meta_entry file meta =
    let oc = open_out file in
    Printf.fprintf oc "meta\n";
    Printf.fprintf oc "%s\n" meta.meta_name;
    Printf.fprintf oc "%s\n" meta.meta_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim meta.meta_opam.opam_version);
    close_out oc

  let save_library_entry file lib =
    let oc = open_out file in
    Printf.fprintf oc "library\n";
    Printf.fprintf oc "%s\n" lib.lib_name;
    Printf.fprintf oc "%s\n" lib.lib_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim lib.lib_opam.opam_version);
    close_out oc

  let save_module_entry file mdl =
    let oc = open_out file in
    Printf.fprintf oc "module\n";
    Printf.fprintf oc "%s\n" mdl.mdl_name;
    Printf.fprintf oc "%s\n" mdl.mdl_opam.opam_name;
    Printf.fprintf oc "%s\n" (String.trim mdl.mdl_opam.opam_version);
    Printf.fprintf oc "%s\n" mdl.mdl_basename;
    StringMap.iter (fun _ lib ->
        Printf.fprintf oc "%s@%s.%s\n"
          lib.lib_name lib.lib_opam.opam_name
          (String.trim lib.lib_opam.opam_version)
      ) mdl.mdl_libs;
    close_out oc
end

open TYPES

let module_cut m =
  let rec iter m i len =
    if i+1 = len then
      m, ""
    else
    if m.[i] = '_' && m.[i+1] = '_' then
      (* Don't forget to capitalize (to handle for instance Stdlib__map) *)
      String.sub m 0 i, String.capitalize (String.sub m (i+2) (len - i - 2))
    else
      iter m (i+1) len
  in
  iter m 0 (String.length m)

let pkg_of_opam opam =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s@%s.%s"
    lib.lib_name lib.lib_opam_name lib.lib_opam_version

let pkg_of_meta meta =
  Printf.sprintf "META.%s@%s.%s"
    meta.meta_name meta.meta_opam_name meta.meta_opam_version

let pkg_of_src src = 
  Printf.sprintf "%s.%s"
    src.src_opam_name src.src_opam_version

let pkg_of_mdl mdl =
  let version = mdl.mdl_opam_version in
  match mdl.mdl_libs with
  | lib :: _rem -> pkg_of_lib lib
  | [] ->
      let pack, alias = module_cut mdl.mdl_basename in
      if alias = "" then
        Printf.sprintf "MODULE.%s@%s.%s"
          mdl.mdl_basename mdl.mdl_opam_name version
      else
        let pkg =
          Printf.sprintf "MODULE.%s__@%s.%s" pack mdl.mdl_opam_name version in
        if Sys.file_exists (Html.digodoc_html_dir // pkg) then
          pkg
        else
          Printf.sprintf "MODULE.%s@%s.%s" pack mdl.mdl_opam_name version

let library_of_string s =
  let lib_name, s = EzString.cut_at s '@' in
  let lib_opam_name, lib_opam_version = EzString.cut_at s '.' in
  { lib_name ; lib_opam_name ; lib_opam_version }

let read_entry file =
  match EzFile.read_lines_to_list file with
  |
    "opam" ::
    opam_name ::
    opam_version ::
    opam_synopsis ->
      let opam_synopsis = String.concat " " opam_synopsis in
      Opam { opam_name ; opam_version ; opam_synopsis }
  | [
    "meta" ;
    meta_name ;
    meta_opam_name ;
    meta_opam_version ;
  ] -> Meta { meta_name ; meta_opam_name ; meta_opam_version }
  | [
    "library" ;
    lib_name ;
    lib_opam_name ;
    lib_opam_version ;
  ] -> Library { lib_name ; lib_opam_name ; lib_opam_version }
  | "module" ::
    mdl_name ::
    mdl_opam_name ::
    mdl_opam_version ::
    mdl_basename ::
    mdl_libs ->
      let mdl_libs = List.map library_of_string mdl_libs in
      Module { mdl_name ; mdl_opam_name ; mdl_opam_version ;
               mdl_basename ; mdl_libs }
  | _lines ->
      Printf.eprintf "Unrecognized format for entry file %S\n%!" file;
      raise Not_found



(*
open Ez_html.V1
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

let pkg_of_opam opam =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s@%s.%s"
    lib.lib_name lib.lib_opam.opam_name lib.lib_opam.opam_version

let pkg_of_meta meta =
  Printf.sprintf "META.%s@%s.%s"
    meta.meta_name meta.meta_opam.opam_name meta.meta_opam.opam_version

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
*)

let print_index bb index entity_name =
  let map = ref StringMap.empty in
  let n = ref 0 in
  List.iter (fun (entry, line) ->
      incr n;
      let i = String.make 1 ( Char.lowercase entry.[0] ) in
      match StringMap.find i !map with
      | exception Not_found ->
          let r = ref [ entry, line ] in
          map := StringMap.add i r !map
      | r -> r := ( entry, line ) :: !r
    ) index;

  Printf.bprintf bb {|
    <h4 id="item-number">%d %s</h4>
    <div class="by-name">
      <nav>
|} !n entity_name;
  StringMap.iter (fun i _ ->
      Printf.bprintf bb {|<a href="#name-%s">%s</a>
|} i i) !map;

  Printf.bprintf bb {|
      </nav>
|};
  StringMap.iter (fun i r ->
      Printf.bprintf bb {|
     <div class="packages-set">
      <h3 id="name-%s">
        <a href="#name-%s" aria-hidden="true" class="anchor">
        </a>%s
      </h3>
      <ol class="packages">
|} i i i ;
      List.iter (fun ( _entry, line ) ->
          Printf.bprintf bb "%s\n" line;
        ) ( List.sort compare !r ) ;

      Printf.bprintf bb {|
      </ol>
     </div>
|};
    ) !map;

  Printf.bprintf bb {|
    </div>
|};
  ()

(*
type modul = {
  modul : ocaml_mdl ;
  mutable modul_subs : ocaml_mdl StringMap.t ;
}

let module_cut m =
  let rec iter m i len =
    if i+1 = len then
      m, ""
    else
    if m.[i] = '_' && m.[i+1] = '_' then
      String.sub m 0 i, String.sub m (i+2) (len - i - 2)
    else
      iter m (i+1) len
  in
  iter m 0 (String.length m)

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
                  let m, sub = module_cut mdl.mdl_name in
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
                   Printf.sprintf " .{ %s }"
                     ( String.concat ", "
                         ( List.map (fun (name, mdl) ->
                               let pkg = pkg_of_mdl mdl in
                               Printf.sprintf
                                 {|<a href="../%s/%s/index.html">%s</a>|}
                                 pkg mdl.mdl_name name
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

  let copy_file file =
    let basename = Filename.basename file in
    let dstfile = html_dir // basename in
    let content =
      EzFile.read_file ( state.opam_switch_prefix // file ) in
    EzFile.make_dir ~p:true html_dir ;
    EzFile.write_file dstfile content;
    Printf.sprintf {|<a href="%s">%s</a>|} basename basename
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
  (
    List.map (function
        | README_md file -> [ "readme-file" ; copy_file file ]
        | CHANGES_md file -> [ "changes-file" ; copy_file file ]
        | LICENSE_md file -> [ "license-file" ; copy_file file ]
        | ODOC_PAGE file -> [ "odoc-file" ; file ]
      ) opam.opam_docs
  )


let save_line ~dir_name ~line_name line =

  let mdl_dir = Html.digodoc_html_dir // dir_name in
  EzFile.make_dir ~p:true mdl_dir;
  EzFile.write_file ( mdl_dir // line_name )
    line;
  ()

*)

let generate_library_index state bb =

  let index = ref [] in

  List.iter (function
      | Library lib ->

          let pkg = pkg_of_lib lib in
          let opam_pkg = pkg_of_opam
              { opam_name = lib.lib_opam_name ;
                opam_version = lib.lib_opam_version ;
                opam_synopsis = "" } in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="%s/index.html" class="digodoc-lib"><code>%s</code></a> in opam <a href="%s/index.html" class="digodoc-opam">%s.%s</a></li>|}
              search_id
              pkg lib.lib_name
              opam_pkg
              lib.lib_opam_name
              lib.lib_opam_version
          in
          index := ( lib.lib_name, line ) :: !index;
      | _ -> ()
    ) state ;

  print_index bb !index "libraries";

  ()

let generate_opam_index state bb =

  let index = ref [] in

  List.iter (function
      | Opam opam ->

          let pkg =
            Printf.sprintf "OPAM.%s.%s" opam.opam_name opam.opam_version in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="%s/index.html" class="digodoc-opam"><code>%s.%s</code></a> %s</li>|}
              search_id
              pkg
              opam.opam_name
              opam.opam_version
              ( Html.encode opam.opam_synopsis )
          in
          index := (opam.opam_name, line ) :: !index;

      | _ -> ()
    ) state ;

  print_index bb !index "packages";
  ()


let generate_module_index state bb =

  let index = ref [] in

  let add_module pack alias mdl =
    let pkg = pkg_of_mdl mdl in
    let opam_pkg = pkg_of_opam {
        opam_name = mdl.mdl_opam_name ;
        opam_version = mdl.mdl_opam_version ;
        opam_synopsis = "" ;
      }
    in

    let search_id = Printf.sprintf "%s:%s" pkg mdl.mdl_name in

    let html_path, mdl_name =
      if alias = "" then
        Printf.sprintf "%s/%s" pkg mdl.mdl_name, mdl.mdl_name
      else
        (* In general, when we have a packed module M__N,
           M is generated and contains an alias N = M__N.
           However, when M already exists (written by the user),
           then the generated module is called M__. *)
        let path = Printf.sprintf "%s/%s__/%s" pkg pack alias in
        if Sys.file_exists (Html.digodoc_html_dir // path) then
          path, Printf.sprintf "%s__.%s" pack alias
        else
          Printf.sprintf "%s/%s/%s" pkg pack alias,
          Printf.sprintf "%s.%s" pack alias
    in

    let line =
      Printf.sprintf
        {|<li class="package" id="%s"><a href="%s/index.html"><code>%s</code></a> in opam <a href="%s/index.html" class="digodoc-opam">%s.%s</a>%s</li>|}
        search_id
        html_path
        mdl_name
        opam_pkg
        mdl.mdl_opam_name
        mdl.mdl_opam_version
        (match mdl.mdl_libs with
         | [] -> ""
         | libs ->
           Printf.sprintf " in libs %s"
             ( String.concat ", "
                 (List.map (fun lib ->
                      Printf.sprintf
                        {|<a href="%s/index.html" class="digodoc-lib">%s</a>|}
                        (pkg_of_lib lib) lib.lib_name
                    ) libs ))
        )
    in
    index := ( mdl.mdl_name, line ) :: !index;
  in

  List.iter (function
      | Module mdl ->
          let pack, alias = module_cut mdl.mdl_name in
          add_module pack alias mdl
      | _ -> ()
    ) state ;

  print_index bb !index "modules";
  ()

let generate_meta_index state bb =


  let index = ref [] in

  List.iter ( function
      | Meta meta ->

          let pkg = pkg_of_meta meta in
          let opam_pkg = pkg_of_opam  {
              opam_name = meta.meta_opam_name ;
              opam_version = meta.meta_opam_version ;
              opam_synopsis = "" ;
            }
          in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="%s/index.html"><code>%s</code></a> in opam <a href="%s/index.html" class="digodoc-opam">%s.%s</a></li>|}
              search_id
              pkg meta.meta_name
              opam_pkg
              meta.meta_opam_name
              meta.meta_opam_version
          in

          index := ( meta.meta_name , line ) :: !index;

      | _ -> ()
    ) state ;

  print_index bb !index "metas";
  ()

let generate_source_index state bb =

  let index = ref [] in

  List.iter ( function
      | Source src ->
          let pkg = pkg_of_src src in
          let opam_pkg = pkg_of_opam  {
              opam_name = src.src_opam_name ;
              opam_version = src.src_opam_version ;
              opam_synopsis = "" ;
            }
          in
          let search_id = pkg in
          let line =
            Printf.sprintf
              {|<li class="package" id="%s"><a href="../sources/%s/index.html"><code>%s</code></a> in opam <a href="%s/index.html" class="digodoc-opam">%s.%s</a></li>|}
              search_id
              pkg src.src_opam_name
              opam_pkg
              src.src_opam_name
              src.src_opam_version
          in

          index := ( src.src_opam_name , line ) :: !index;

      | _ -> ()
    ) state ;

  print_index bb !index "sources";
  ()

let read_all_entries () =
  let entries = ref [] in
  let dir = Html.digodoc_html_dir in
  Array.iter (fun pkg ->
      let dir = dir // pkg in
      Array.iter (fun file ->

          if EzString.starts_with file ~prefix:"ENTRY." then
            let entry = read_entry ( dir // file ) in
            begin  
              match entry with
              | Opam {opam_name; opam_version; _ } ->
                let src = Source {src_opam_name=opam_name;src_opam_version=opam_version} in
                entries := src :: !entries
              | _ -> ()
            end;
            entries := entry :: !entries

        ) ( try Sys.readdir dir with _ -> [||] )
    ) ( Sys.readdir dir ) ;

  Printf.eprintf "%d entries read\n%!" ( List.length !entries ) ;
  !entries

let generate () =
  Printf.eprintf "Generating index...\n%!";

  let state = read_all_entries () in

  let stdlib_version = Option.value ~default:"4.10.0" @@ List.find_map (function
      | Library {lib_opam_name = "ocaml-base-compiler" ; lib_opam_version; _} ->
          Some lib_opam_version
      | _ -> None) state in
  let header bb ~title =
    Printf.bprintf bb
      {|
  <h1>OCaml Documentation: %s</h1>
  <h2>OCaml Distribution</h2>
  <ul>
    <li><a href="https://caml.inria.fr/pub/docs/manual-ocaml/">OCaml Manual</a></li>
    <li><a href="LIBRARY.stdlib@ocaml-base-compiler.%s/Stdlib/index.html#modules">Stdlib Modules</a></li>
  </ul>
  <nav class="toc">
  <ul>
    <li><a href="https://caml.inria.fr/pub/docs/manual-ocaml/">OCaml Manual</a></li>
    <li><a href="LIBRARY.stdlib@ocaml-base-compiler.%s/Stdlib/index.html#modules">Stdlib Modules</a></li>
  </ul>
  </nav>
  </header>
  <h2>Index</h2>
|} title stdlib_version stdlib_version;
  in

  let trailer _bb =
    ()
  in
  Html.generate_page
    ~filename:"index.html"
    ~title:"Main Index"
    (fun bb ~title ->
       header bb ~title;
       generate_opam_index state bb;
       trailer bb;
    );

  Html.generate_page
    ~filename:"libraries.html"
    ~title:"Libraries Index"
    (fun bb ~title  ->
       header bb ~title;
       generate_library_index state bb;
       trailer bb;
    );

  Html.generate_page
    ~filename:"metas.html"
    ~title:"Meta Index"
    (fun bb ~title ->
       header bb ~title;
       generate_meta_index state bb;
       trailer bb;
    );

  Html.generate_page
    ~filename:"modules.html"
    ~title:"Modules Index"
    (fun bb ~title ->
       header bb ~title;
       generate_module_index state bb;
       trailer bb;
    );
  if !Htmlize.Globals.sources then begin
    Html.generate_page
      ~filename:"sources.html"
      ~title:"Sources Index"
      (fun bb ~title ->
        header bb ~title;
        generate_source_index state bb;
        trailer bb;
      )
  end;

  Printf.eprintf "Index generation done.\n%!";
  ()

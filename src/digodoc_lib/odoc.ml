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


let pkg_of_opam opam =
  Printf.sprintf "OPAM.%s.%s"
    opam.opam_name opam.opam_version

let pkg_of_lib lib =
  Printf.sprintf "LIBRARY.%s.%s.%s"
    lib.lib_name lib.lib_opam.opam_name lib.lib_opam.opam_version

let pkg_of_meta meta =
  Printf.sprintf "META.%s.%s.%s"
    meta.meta_name meta.meta_opam.opam_name meta.meta_opam.opam_version

let pkg_of_mdl mdl =
  let version = mdl.mdl_opam.opam_version in
  match StringMap.bindings mdl.mdl_libs with
  | (_, lib) :: _rem -> pkg_of_lib lib
  | [] ->
      Printf.sprintf "MODULE.%s.%s.%s"
        mdl.mdl_basename mdl.mdl_opam.opam_name version

let digodoc_dir = "_digodoc"
let digodoc_html_dir = digodoc_dir // "html"
let digodoc_odoc_dir = digodoc_dir // "odoc"

let force_rebuild = match Sys.getenv "FORCE_REBUILD" with
  | exception Not_found -> false
  | _ -> true

let skip_modules = match Sys.getenv "SKIP_MODULES" with
  | exception Not_found -> false
  | _ -> true

let call_odoc state mdl ~pkgs ext =
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

    Process.call ( Array.of_list cmd );
  end;

  let cmd = [
    "odoc" ; "html" ;
    "--theme-uri"; "_odoc-theme" ;
    "-o" ; digodoc_html_dir ;
    odoc_target ]
    @ includes
  in

  Process.call ( Array.of_list cmd );
  ()

let lookup_cmi state ~name ~crc =
  try Hashtbl.find state.ocaml_mdls_by_cmi_crc crc with
  | Not_found ->
      match Hashtbl.find_all state.ocaml_mdls_by_name name with
      | [ mdl ] -> mdl
      | _ -> raise Not_found


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

  let rec check mdl =
    if not ( Hashtbl.mem done_table mdl.mdl_longname ) then begin
      Hashtbl.add done_table mdl.mdl_longname ();
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
                    check dep_mdl
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
                    check dep_mdl
              ) unit.unit_import_cmis;
            StringMap.iter (fun name crc ->
                match Hashtbl.find state.ocaml_mdls_by_cmx_crc crc with
                | exception Not_found ->
                    Printf.eprintf
                      "mdl %s depends on unfound cmx %s with crc %s\n"
                      mdl.mdl_longname name crc
                | dep_mdl ->
                    check dep_mdl
              ) unit.unit_import_cmxs;
      end;
      f state mdl
    end
  in
  Hashtbl.iter (fun _ mdl ->
      check mdl
    ) state.ocaml_mdls_by_cmi_crc


let generate_library_index state bb =

  Printf.bprintf bb {|
  <h1 id="index"><a href="#index-libraries" class="anchor"></a>Index of Libraries</h1>
  <ul class="libraries">
|};

  List.iter (fun  ( _, lib ) ->
      Printf.eprintf "For library %s\n%!" lib.lib_name ;
      let pkg = pkg_of_lib lib in
      let opam_pkg = pkg_of_opam lib.lib_opam in
      Printf.bprintf bb
        {|<li><a href="%s/index.html"><code>%s</code></a> in opam <a href="%s/index.html">%s.%s</a></li>|}
        pkg lib.lib_name
        opam_pkg
        lib.lib_opam.opam_name
        lib.lib_opam.opam_version;

      let b = Buffer.create 10000 in

      Printf.bprintf b "{0:library-%s Library %s\n"
        lib.lib_name lib.lib_name ;
      Printf.bprintf b
        {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
        opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
      Printf.bprintf b "}\n";

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
                Printf.sprintf {|<a href="../%s/index.html">%s</a>|}
                  (pkg_of_meta meta)
                  meta.meta_name
              )
           ));
      Printf.bprintf b "</table>%%}\n";

      Printf.bprintf b "{1:modules Library modules}\n";
      Printf.bprintf b "{!modules:\n";
      StringMap.iter (fun _ mdl ->
          Printf.bprintf b "  %s\n" mdl.mdl_name;
        ) lib.lib_mdls;
      Printf.bprintf b "}\n";

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
        "-o" ; digodoc_html_dir ;
        "-I" ; digodoc_odoc_dir // pkg ;
        odoc_target
      ]
      in

      Process.call ( Array.of_list cmd );

      ()
    )
    ( List.sort compare state.ocaml_libs );


  Printf.bprintf bb
    {|
   </ul>
|};
  ()


let generate_opam_index state bb =
  Printf.bprintf bb {|
  <h1 id="index"><a href="#index-opam" class="anchor"></a>Index of Opam Packages</h1>
  <ul class="opams">
|};

  StringMap.iter (fun _ opam ->
      let pkg = pkg_of_opam opam in
      Printf.bprintf bb
        {|<li><a href="%s/index.html"><code>%s.%s</code></a> %s</li>|}
        pkg opam.opam_name
        opam.opam_version
        (match opam.opam_synopsis with
         | None -> ""
         | Some s -> s); (* TODO html encode *)

      let b = Buffer.create 10000 in

      Printf.bprintf b "{0:opam-%s.%s Opam Package %s.%s\n"
        opam.opam_name opam.opam_version
        opam.opam_name opam.opam_version;
      (*
      Printf.bprintf b
        {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
        opam_pkg lib.lib_opam.opam_name lib.lib_opam.opam_version;
*)
      Printf.bprintf b "}\n";

      Printf.bprintf b "{1:info Package info}\n";

      Printf.bprintf b {|{%%html:<table class="package info">|};
      (*
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
*)
      Printf.bprintf b "</table>%%}\n";


      Printf.bprintf b "{1:modules Package modules}\n";
      Printf.bprintf b "{!modules:\n";
      StringMap.iter (fun _ mdl ->
          Printf.bprintf b "  %s\n" mdl.mdl_name;
        ) opam.opam_mdls;
      Printf.bprintf b "}\n";


      Printf.bprintf b "{1:files Package files}\n";
      Printf.bprintf b {|{%%html:<pre>|};
      List.iter (fun (file, _) ->
          Printf.bprintf b "%s\n" file
        ) opam.opam_files;
      Printf.bprintf b {|</pre>%%}|};

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
        "-o" ; digodoc_html_dir ;
        "-I" ; digodoc_odoc_dir // pkg ;
        odoc_target
      ] @
        List.flatten (List.map (fun (_,lib) ->
            [ "-I" ; digodoc_odoc_dir // pkg_of_lib lib ]
          ) (StringMap.to_list opam.opam_libs))
      in

      Process.call ( Array.of_list cmd );

      ()
    ) state.opam_packages ;

  Printf.bprintf bb
    {|
   </ul>
|};
  ()

let generate_module_index state bb =

  let list = ref [] in

  List.iter (fun ( _ , mdl ) ->
      if StringSet.mem "cmi" mdl.mdl_exts then
        begin
          list := ( mdl.mdl_name, false, mdl) :: !list;
          let rec iter s i n =
            if i = String.length s then
              ()
            else
            if s.[i] = '_' then
              if n = 1 then
                let s = String.sub s (i+1) (String.length s - i - 1) in
                if s = "" then
                  ()
                else
                  list := ( String.capitalize s, true, mdl) :: !list
              else
                iter s (i+1) 1
            else
              iter s (i+1) 0
          in
          iter mdl.mdl_name 0 0
        end
    ) state.ocaml_mdls ;


  let list = List.sort compare !list in


  Printf.bprintf bb {|
  <h1 id="index"><a href="#index-module" class="anchor"></a>Index of modules</h1>
  <ul class="modules">
|};

  List.iter (fun ( short_name , long, mdl ) ->
      let pkg = pkg_of_mdl mdl in
      let opam = mdl.mdl_opam in
      let opam_pkg = pkg_of_opam opam in
      Printf.bprintf bb
        {|<li><a href="%s/%s/index.html"><code>%s</code></a>%s in opam <a href="%s/index.html">%s.%s</a>%s</li>|}
        pkg
        mdl.mdl_name
        short_name
        (if long then
           Printf.sprintf " alias of %s" mdl.mdl_name
         else
           ""
        )
        opam_pkg
        opam.opam_name
        opam.opam_version
        (if StringMap.is_empty mdl.mdl_libs then
           ""
         else
           Printf.sprintf " in libs %s"
             ( String.concat ", "
                 ( StringMap.to_list mdl.mdl_libs |>
                   List.map (fun (_,lib) ->
                       Printf.sprintf {|<a href="%s/index.html">%s</a>|}
                         (pkg_of_lib lib) lib.lib_name
                     ) ))
        )
    ) list ;

  Printf.bprintf bb
    {|
   </ul>
|};

  ()

let generate_meta_index state bb =

  Printf.bprintf bb {|
  <h1 id="index"><a href="#index-meta" class="anchor"></a>Index of Dune/OCamlfind Packages</h1>
  <ul class="metas">
|};

  StringMap.iter (fun _ meta ->
      let pkg = pkg_of_meta meta in
      let opam = meta.meta_opam in
      let opam_pkg = pkg_of_opam opam in
      Printf.bprintf bb
        {|<li><a href="%s/index.html"><code>%s</code></a> in opam <a href="%s/index.html">%s.%s</a></li>|}
        pkg meta.meta_name
        opam_pkg
        opam.opam_name
        opam.opam_version;

      let b = Buffer.create 10000 in

      Printf.bprintf b "{0:opam-%s Dune/OCamlfind Package %s\n"
        meta.meta_name meta.meta_name;
      Printf.bprintf b
        {|{%%html:<nav><a href="../%s/index.html">%s.%s</a></nav>%%}|}
        opam_pkg opam.opam_name opam.opam_version;
      Printf.bprintf b "}\n";

      Printf.bprintf b "{1:info Package info}\n";
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

      Printf.bprintf b "{1:modules Library modules}\n";
      Printf.bprintf b "{!modules:\n";
      StringMap.iter (fun _ mdl ->
          Printf.bprintf b "  %s\n" mdl.mdl_name;
        ) lib.lib_mdls;
      Printf.bprintf b "}\n";
*)
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
        "-o" ; digodoc_html_dir ;
        "-I" ; digodoc_odoc_dir // pkg ;
        odoc_target
      ]
      in

      Process.call ( Array.of_list cmd );

      ()
    ) state.meta_packages ;

  Printf.bprintf bb
    {|
   </ul>
|};
  ()

let generate ~state =

  (* Iter on modules first *)

  EzFile.make_dir ~p:true digodoc_html_dir;
  Process.call [|
    "rsync"; "-auv"; "html/.";  digodoc_html_dir // "." |];

  let deps_of_pkg = deps_of_pkg state in

  if force_rebuild || not skip_modules then begin

    iter_modules_with_cmi state (fun state mdl ->
        let pkg = pkg_of_mdl mdl in
        let pkgs = Hashtbl.find deps_of_pkg pkg in
        let pkgs = StringSet.to_list !pkgs in
        if StringSet.mem "cmti" mdl.mdl_exts then
          call_odoc state mdl ~pkgs ".cmti"
        else
        if StringSet.mem "cmt" mdl.mdl_exts then
          call_odoc state mdl ~pkgs ".cmt"
        else
        if StringSet.mem "cmi" mdl.mdl_exts then
          call_odoc state mdl ~pkgs ".cmi"
      )
  end;

  (* TODO:
     * Separate in multiple index files ?
     * Create buckets per initial letter
  *)

  (* Iter on every library *)

  let bb = Buffer.create 10000 in
  Printf.bprintf bb {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
 <head>
  <title>index</title>
  <link rel="stylesheet" href="_odoc-theme/odoc.css"/>
  <meta charset="utf-8"/>
  <meta name="generator" content="digodoc 0.1"/>
  <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  <script src="highlight.pack.js"></script>
  <script>hljs.initHighlightingOnLoad();</script>
</head>
|};
  Printf.bprintf bb
    {|
<body>
 <div class="content">
  <header><nav>Index</nav>
|};

  generate_library_index state bb;
  generate_opam_index state bb;
  generate_meta_index state bb;
  generate_module_index state bb;

  Printf.bprintf bb
    {|
  </header>
 </div>
</body>
</html>
|};
  let contents = Buffer.contents bb in
  EzFile.write_file (digodoc_html_dir // "index.html") contents;
  ()

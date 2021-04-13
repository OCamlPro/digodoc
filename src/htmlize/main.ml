(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2021 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

open Ez_html.V1
open Ez_subst.V1
open Ezcmd.V2
(* open EZCMD.TYPES *)
open EzFile.OP

let string_of_color color =
  let open Color in
  match color with
    | TEXT -> "t"
    | COMMENT -> "c"
    | KEYWORD -> "k"
    | STRING -> "s"
    | NUMBER -> "n"
    | CHAR -> "ch"
    | MODULE -> "m"
    | LABEL -> "l"
    | FUNCTION -> "f"
    | ARGUMENT -> "a"
    | TYPE -> "typ"
    | SYNTAX -> "syn"

let pkg_of_opam opam_name opam_version =
  Printf.sprintf "OPAM.%s.%s"opam_name opam_version

let is_directory file =
  match Unix.lstat file with
  | exception _ -> false
  | st -> st.Unix.st_kind = Unix.S_DIR

let htmlize filename content =
  let b = Buffer.create 1000 in
  Printf.bprintf b {|<div class="wrap-x padding"><table class="content-table">
 <tbody>
|};
  let lines = Color.file filename content in

  List.iter (fun (i, line) ->
      Printf.bprintf b {|  <tr class="line">
|};
      Printf.bprintf b {|   <td id="L%d" class="line-num">%d</td>
|} i i;
      Printf.bprintf b {|   <td id="LC%d" class="line-code">|} i;

      List.iter (fun (color, s) ->
          let s = HTML.encode s in
          match color with
          | Color.TEXT -> Buffer.add_string b s
          | _ -> Printf.bprintf b {|<span class="sp-%s">%s</span>|}
                  (string_of_color color) s
        ) line;

      Printf.bprintf b {|</td>|};
      Printf.bprintf b {|  </tr>
|};
    ) lines;
  Printf.bprintf b {| </tbody>
</table></div>
|};
  Buffer.contents b

let content_info content =
  let lines = EzString.split content '\n' in
  Printf.sprintf {|%d lines <span class="separator">|</span> %d chars|}
    (List.length lines) (String.length content)

let generate_page ~brace destdir =
  let ctxt = () in
  let header = 
    if !Globals.with_header
    then Files.html_header ^ (Files.body_header ()) 
    else Files.html_header
  in
  let header = EZ_SUBST.string header ~ctxt ~brace in
  let trailer = EZ_SUBST.string (Files.html_trailer ()) ~ctxt ~brace in
  let page = EZ_SUBST.string Files.html_file_page ~ctxt ~brace in

  EzFile.make_dir ~p:true destdir;
  EzFile.write_file ( destdir // "index.html" )
    ( Printf.sprintf "%s%s%s" header page trailer );
  ()

let escape_file file =
  match file with
  | "index.html" -> "index.html_"
  | _ -> file

let title_info path =

  let rec iter list =
    match list with
    | [] -> assert false
    | [ file ] -> [ Printf.sprintf "<b>%s</b>"
                      (HTML.encode file) ]
    | dir :: file ->
        let s =
          Printf.sprintf "<a href='%s/index.html'>%s</a>"
            (String.concat "/" (List.map (fun _s -> "..") file))
            (HTML.encode dir)
        in
        s :: iter file
  in
  let path_html =
    String.concat "/"
      (List.map (fun _s -> "..") path)
  and opam_name,opam_version = EzFile.cut_extension (List.hd path) in
  let pkg_link =      
    Printf.sprintf {| <a class="digodoc-opam" href="%s/../html/%s/index.html">package</a>|}
      path_html
      (pkg_of_opam opam_name opam_version)
  in
    String.concat {| <span class="separator">/</span> |} (pkg_link::iter path)


let htmlize_file destdir srcdir path file =

  let path = path @ [ file ] in
  let srcfile = srcdir // file in
  let destdir = destdir // (escape_file file) in
  let rawdir = destdir // "raw" in
  let content = try
      EzFile.read_file srcfile
    with exn ->
      Printf.kprintf failwith "EzFile.read_file('%s'): %s"
        srcfile ( Printexc.to_string exn)
  in
  EzFile.make_dir ~p:true rawdir;
  EzFile.write_file ( rawdir // file ) content ;

  let brace () var = match var with
    | "content" -> htmlize file content
    | "content-info" -> content_info content
    | "title" -> String.concat "/" path
    | "title-info" -> title_info path
    | "root" ->
        let s =
          String.concat "/"
            (List.map (fun _s -> "..") path)
        in
        if s = "" then s else s ^ "/"
    | "root-html" -> 
        let s =
          String.concat "/"
            (List.map (fun _s -> "..") path)
        in
        let s = if s = "" then s else s ^ "/" in
        s // "../html/"
    | _ ->
        Printf.kprintf failwith "Unknown var %S" var
  in
  generate_page ~brace destdir

let dir_content srcdir files path =
  let b = Buffer.create 1000 in
  
  let has_parent_directory =
    match path with
    | [] -> assert false
    | [_dir] -> false
    | _ -> true
  in
  if has_parent_directory then begin
    Printf.bprintf b {|<div class="files-div">
  |};

    Printf.bprintf b {|  <a class="file-link" href='../index.html'><div class="file">
  |};
    Printf.bprintf b {|   <table class="file-tab"><tbody><tr>
  |};
    Printf.bprintf b {|     <td class="file-icon">%s</td>
  |} Files.svg_directory;
  
    Printf.bprintf b {|     <td class="file-name">..</td>
  |};
    Printf.bprintf b {|     <td class="file-kind">Upper Directory</td>
  |};
    Printf.bprintf b {|   </tr></tbody></table>
  |};
    Printf.bprintf b {|  </div></a>|};
  end;
  let files = Array.to_list files in
  let files = List.map (fun file ->
      let filename = srcdir // file in
      let st = Unix.lstat filename in

      st.st_kind <> Unix.S_DIR, file, st) files
  in
  let files = List.sort compare files in
  List.iteri (fun i (_, file, st) ->
      let style =
        if i = 0 && not has_parent_directory
        then "file" 
        else if i = (List.length files) - 1 
        then "file top-border round-border"
        else "file top-border"
      in
      Printf.bprintf b {|  <a class="file-link" href='%s/index.html'><div class='%s'>
        |} (HTML.encode (escape_file file)) style;
      
      Printf.bprintf b {|   <table class="file-tab"><tbody><tr>
        |};

      (match st.Unix.st_kind with
       | Unix.S_DIR ->
           Printf.bprintf b {|  <td class="file-icon">%s</td>
          |} Files.svg_directory
       | _ ->
           Printf.bprintf b {|  <td class="file-icon">%s</td>
          |} Files.svg_file
      );

      (match st.Unix.st_kind with
       | Unix.S_DIR | Unix.S_REG ->
           Printf.bprintf b {|   <td class="file-name">%s</td>
          |} (HTML.encode file);
       | _ ->
           Printf.bprintf b {|   <td class="file-name">%s</td>
          |} (HTML.encode file);
      );
      Printf.bprintf b {|   <td class="file-kind">%s</td>
      |}
        (match st.Unix.st_kind with
         | Unix.S_REG ->
             Printf.sprintf "%d bytes" st.Unix.st_size
         | Unix.S_DIR ->
             "Directory"
         | _ -> "???");
     Printf.bprintf b {|   </tr></tbody></table>
    |};
  Printf.bprintf b {|  </div></a>
  |};
    ) files;
  Printf.bprintf b {|</div>
  |};
  Buffer.contents b

let dir_info files = Printf.sprintf {|%d files|} (Array.length files)

let rec htmlize_dir destdir srcdir path basename =
  let path = path @ [ basename ] in
  let srcdir = srcdir // basename in
  let destdir = destdir // basename in

  let files = Sys.readdir srcdir in
  Array.sort compare files;

  let brace () var = match var with
    | "content" -> dir_content srcdir files path
    | "content-info" -> dir_info files
    | "title" -> String.concat "/" path
    | "title-info" -> title_info path
    | "root" ->
        let s =
          String.concat "/"
            (List.map (fun _s -> "..") path)
        in
        if s = "" then s else s ^ "/"
    | "root-html" -> 
        let s =
          String.concat "/"
            (List.map (fun _s -> "..") path)
        in
        let s = if s = "" then s else s ^ "/" in
        s // "../html/"  
    | _ ->
        Printf.kprintf failwith "Unknown var %S" var
  in
  generate_page ~brace destdir;
  Array.iter (fun file ->
      let filename = srcdir // file in
      let st = Unix.lstat filename in
      match st.Unix.st_kind with
      | Unix.S_DIR ->
          htmlize_dir destdir srcdir path file
      | Unix.S_REG ->
          htmlize_file destdir srcdir path file
      | _ -> ()
    ) files

let htmlize_dir destdir dir =
  let dirname = Filename.dirname dir in
  let dirname = if dirname = "." then "" else dirname in
  let basename = Filename.basename dir in
  htmlize_dir destdir dirname [] basename

let htmlize target_dir dirs =
  EzFile.make_dir ~p:true target_dir;
  let static_dir = target_dir // "_static" in
  EzFile.make_dir ~p:true static_dir;
  EzFile.write_file ( static_dir // "style.css" ) Files.style_css;
  EzFile.write_file ( static_dir // "script.js" ) Files.script_js;

  List.iter (htmlize_dir target_dir) dirs;
  ()


let main () =
  let dirs = ref [] in
  let target_dir = ref "_html" in
  Printexc.record_backtrace true;
  EZCMD.parse EZCMD.TYPES.[
    "--target", Arg.String (fun s -> target_dir := s),
    "Target directory";
  ] (fun dir -> dirs := dir :: !dirs)
    "htmlize [OPTIONS] DIRS";

  let target_dir = !target_dir in
  let dirs = List.rev !dirs in

  htmlize target_dir dirs

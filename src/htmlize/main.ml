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


let is_directory file =
  match Unix.lstat file with
  | exception _ -> false
  | st -> st.Unix.st_kind = Unix.S_DIR

let htmlize filename content =
  let b = Buffer.create 1000 in
  Printf.bprintf b {|<div class="content-div wrap-x"><table class="content-table">
 <tbody>
|};
  let lines = Color.file filename content in

  List.iter (fun (i, line) ->
      let i = i + 1 in
      Printf.bprintf b {|  <tr class="line">
|};
      Printf.bprintf b {|   <td id="L%d" class="line-num">%d</td>
|} i i;
      Printf.bprintf b {|   <td id="LC%d" class="line-code">|} i;

      List.iter (fun (color, s) ->
          let s = HTML.encode s in
          match color with
          | Color.TEXT -> Buffer.add_string b s
          | _ -> Printf.bprintf b {|<span class="sp-%c">%s</span>|}
                   (match color with
                    | TEXT -> 't'
                    | COMMENT -> 'c'
                    | KEYWORD -> 'k'
                    | STRING -> 's'
                    | NUMBER -> 'n'
                    | MODULE -> 'm'
                   ) s
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
  Printf.sprintf "%d lines | %d chars"
    (List.length lines) (String.length content)

let generate_page ~brace destdir =
  let ctxt = () in
  let header = EZ_SUBST.string Files.html_header ~ctxt ~brace in
  let trailer = EZ_SUBST.string Files.html_trailer ~ctxt ~brace in
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
    | [ file ] -> [ HTML.encode file ]
    | dir :: file ->
        let s =
          Printf.sprintf "<a href='%s/index.html'>%s</a>"
            (String.concat "/" (List.map (fun _s -> "..") file))
            (HTML.encode dir)
        in
        s :: iter file
  in
  String.concat " / " (iter path)

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
    | _ ->
        Printf.kprintf failwith "Unknown var %S" var
  in
  generate_page ~brace destdir

let dir_content srcdir files path =
  let b = Buffer.create 1000 in
  Printf.bprintf b {|<div class="files-div"><table class="files-table">
 <tbody class="file">
|};

  Printf.bprintf b {|  <tr class="file">
|};
  Printf.bprintf b {|   <td class="file-icon">%s</td>|} Files.svg_directory;
  let parent_directory =
    match path with
    | [] -> assert false
    | [_dir] -> "."
    | _ -> ".."
  in
  Printf.bprintf b {|   <td class="file-name"><a href='%s/index.html'>&lt;PARENT DIRECTORY&gt;</a></td>
|} parent_directory;
  Printf.bprintf b {|   <td class="file-kind">Upper Directory</td>
|};
  Printf.bprintf b {|  </tr>
|};

  let files = Array.to_list files in
  let files = List.map (fun file ->
      let filename = srcdir // file in
      let st = Unix.lstat filename in

      st.st_kind <> Unix.S_DIR, file, st) files
  in
  let files = List.sort compare files in
  List.iter (fun (_, file, st) ->
      Printf.bprintf b {|  <tr class="file">
|};

      (match st.Unix.st_kind with
       | Unix.S_DIR ->
           Printf.bprintf b {|  <td class="file-icon">%s</td>|} Files.svg_directory
       | _ ->
           Printf.bprintf b {|  <td class="file-icon">%s</td>|} Files.svg_file
      );

      (match st.Unix.st_kind with
       | Unix.S_DIR | Unix.S_REG ->
           Printf.bprintf b {|   <td class="file-name"><a href='%s/index.html'>%s</a></td>|} (HTML.encode (escape_file file)) (HTML.encode file);
       | _ ->
           Printf.bprintf b {|   <td class="file-name">%s</td>|} (HTML.encode file);
      );
      Printf.bprintf b {|   <td class="file-kind">%s</td>
|}
        (match st.Unix.st_kind with
         | Unix.S_REG ->
             Printf.sprintf "%d bytes" st.Unix.st_size
         | Unix.S_DIR ->
             "Directory"
         | _ -> "???");
      Printf.bprintf b {|  </tr>
|};
    ) files;
  Printf.bprintf b {| </tbody>
</table></div>
|};
  Buffer.contents b

let dir_info _files = "FILE"

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

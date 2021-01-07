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

let style_css = {|
body {
  font-family: -apple-system,BlinkMacSystemFont,Segoe UI,Helvetica,Arial,sans-serif,Apple Color Emoji,Segoe UI Emoji;
  font-size: 14px;
  line-height: 1.5;
  color: var(--color-text-primary);
}

/* Add a black background color to the top navigation */
.topnav {
    background-color: #333;
    overflow: hidden;
}

/* Style the links inside the navigation bar */
.topnav a {
  float: left;
  color: #f2f2f2;
  text-align: center;
  padding: 14px 16px;
  text-decoration: none;
  font-size: 17px;
}

.files-table {
  width: 100%;
}

.file {
  width: 100%;
}

.file-name {
  width: 50%;
}

/* Change the color of links on hover */
.topnav a:hover {
  background-color: #ddd;
  color: black;
}

/* Add a color to the active/current link */
.topnav a.active {
  background-color: #4CAF50;
  color: white;
}

/* Right-aligned section inside the top navigation */
.topnav-right {
  float: right;
}

.content {
    max-width: 1280px;
    margin-right: auto;
    margin-left: auto;
}

.border {
  border-bottom-right-radius: 6px;
  border-bottom-left-radius: 6px;
  box-sizing: border-box;
  padding: 16px;
  margin: -1px -1px 0;
  border: 1px solid grey;
  border-top-left-radius: 6px;
  border-top-right-radius: 6px;
}

.position-relative {
  position: relative;
}

.content-table {
  border-color: grey;
}

.file-info {
  margin-top: 20px;
}

.content-info {
  margin-top: 20px;
}

.line-num {
    width: 1%;
    min-width: 50px;
    padding-right: 10px;
    padding-left: 10px;
    font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;
    font-size: 12px;
    line-height: 20px;
    color: var(--color-diff-blob-num-text);
    text-align: right;
    white-space: nowrap;
    vertical-align: top;
    cursor: pointer;
    -webkit-user-select: none;
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;
}

.line-code {
  font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;
  font-size: 12px;
  white-space: pre;

  position: relative;
  padding-right: 10px;
  padding-left: 10px;
  line-height: 20px;
  vertical-align: top;
}
|}
let script_js = {||}

let html_header = {|<!DOCTYPE html>
<html lang="en">
 <head>
   <meta charset="utf-8">
   <title>${title}</title>
   <link rel="stylesheet" href="style.css" />
   <script defer="defer" type="application/javascript" src="script.js"></script>
  </head>
 <body>
<div class="topnav">
  <a class="active" href="#home">Home</a>
  <a href="#news">News</a>
  <a href="#contact">Contact</a>
  <div class="topnav-right">
    <a href="#search">Search</a>
    <a href="#about">About</a>
  </div>
</div>
|}

let html_trailer =
  {|\
 </body>
</html>
|}
let html_file_page = {|
<div class="container position-relative">
 <div class="content">
  <div class="file-info border">${title}</div>
  <div class="content-info border">${content-info}</div>
  <div class="content-div border">${content}</div>
 </div>
</div>
|}

type color =
  | BLACK
  | RED

let color_of_token = function
  | Approx_tokens.LET -> RED
  | _ -> BLACK

let htmlize content =
  let b = Buffer.create 1000 in
  Printf.bprintf b {|<div class="content-div"><table class="content-table">
 <tbody>
|};
  let tokens = Approx_lexer.tokens_of_string content in
  let len = String.length content in
  let colors = Array.make len BLACK in

  List.iter (fun (token, ( (lex_start, lex_end), _, _)) ->
      let lex_start = lex_start.Lexing.pos_cnum in
      let lex_end = lex_end.Lexing.pos_cnum in
      let color = color_of_token token in
      for i = lex_start to lex_end-1 do
        colors.(i) <- color
      done;
    ) tokens;

  let lines = EzString.split content '\n' in
  List.iteri (fun i line ->
      let i = i + 1 in
      Printf.bprintf b {|  <tr class="line">
|};
      Printf.bprintf b {|   <td id="L%d" class="line-num">%d</td>
|} i i;
      Printf.bprintf b {|   <td id="LC%d" class="line-code">%s</td>
|}
        i (HTML.encode line);
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
  let header = EZ_SUBST.string html_header ~ctxt ~brace in
  let trailer = EZ_SUBST.string html_trailer ~ctxt ~brace in
  let page = EZ_SUBST.string html_file_page ~ctxt ~brace in

  EzFile.make_dir ~p:true destdir;
  EzFile.write_file ( destdir // "index.html" )
    ( Printf.sprintf "%s%s%s" header page trailer );
  EzFile.write_file ( destdir // "style.css" ) style_css;
  EzFile.write_file ( destdir // "script.js" ) script_js;
  ()

let escape_file file =
  match file with
  | "index.html" -> "index.html_"
  | "script.js" -> "script.js_"
  | "style.css" -> "style.css_"
  | _ -> file

let title_info dir =

  let list = EzString.split dir '/' in

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
  String.concat " / " (iter list)

let htmlize_file destdir srcdir file =

  let destdir = destdir // (escape_file file) in
  let rawdir = destdir // "raw" in
  let content = EzFile.read_file ( srcdir // file ) in
  EzFile.make_dir ~p:true rawdir;
  EzFile.write_file ( rawdir // file ) content ;

  let brace () var = match var with
    | "content" -> htmlize content
    | "content-info" -> content_info content
    | "title" -> title_info ( srcdir // file )
    | _ ->
        Printf.kprintf failwith "Unknown var %S" var
  in
  generate_page ~brace destdir

let dir_content srcdir files =
  let b = Buffer.create 1000 in
  Printf.bprintf b {|<div class="files-div"><table class="files-table">
 <tbody class="file">
|};

  Printf.bprintf b {|  <tr class="file">
|};
  Printf.bprintf b {|   <td class="file-name"><a href='../index.html'>..</a></td>
|};
  Printf.bprintf b {|   <td class="file-kind">Upper Directory</td>
|};
    Printf.bprintf b {|  </tr>
|};

  let files = Array.to_list files in
  List.iter (fun file ->
      let filename = srcdir // file in
      let st = Unix.lstat filename in
      Printf.bprintf b {|  <tr class="file">
|};
      Printf.bprintf b {|   <td class="file-name"><a href='%s/index.html'>%s</a></td>
|} (HTML.encode (escape_file file)) (HTML.encode file);
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

let rec htmlize_dir destdir srcdir basename =
  let srcdir = srcdir // basename in
  let destdir = destdir // basename in

  let files = Sys.readdir srcdir in
  Array.sort compare files;

  let brace () var = match var with
    | "content" -> dir_content srcdir files
    | "content-info" -> dir_info files
    | "title" -> title_info srcdir
    | _ ->
        Printf.kprintf failwith "Unknown var %S" var
  in
  generate_page ~brace destdir;
  Array.iter (fun file ->
      let filename = srcdir // file in
      if Sys.is_directory filename then
        htmlize_dir destdir srcdir file
      else
        htmlize_file destdir srcdir file
    ) files

let htmlize_dir destdir dir =
  let dirname = Filename.dirname dir in
  let dirname = if dirname = "." then "" else dirname in
  let basename = Filename.basename dir in
  htmlize_dir destdir dirname basename

let () =
  Printexc.record_backtrace true;
  EZCMD.parse [
  ] (htmlize_dir "_html")
    "htmlize [OPTIONS] DIRS"

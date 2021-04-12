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
open EzFile.OP

let digodoc_html_dir = Globals.digodoc_dir // "html"

(* generate page _digodoc/html/${filename} *)
let generate_page ~filename ~title f =

  let dirname = EzFile.dirname filename in 
  let path_list = 
      if String.contains filename '/'   
      then 
        String.split_on_char '/' dirname 
      else [] in
  let s =
    String.concat "/"
      (List.map (fun _s -> "..") 
        path_list) in
  let root = if s = "" then s else s ^ "/" in


  (* removed 'async' from the script line because unregnized by ez_ml parser *)
  let bb = Buffer.create 10000 in
  Printf.bprintf bb {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
 <head>
  <title>%s</title>
  <link rel="stylesheet" href="%s_odoc-theme/odoc.css"/>
  <script type="text/javascript" src="%ssearch.js" charset="utf-8"></script>
  <meta charset="utf-8"/>
  <meta name="generator" content="digodoc 0.1"/>
  <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  <script src="%shighlight.pack.js"></script>
  <script>hljs.initHighlightingOnLoad();</script>
</head>
|} title root root root;
  Printf.bprintf bb
    {|
<body>
 <div class="content">
|};
  f bb ~title;
  Printf.bprintf bb
    {|
 </div>
</body>
</html>
|};
  let contents = Buffer.contents bb in
  EzFile.write_file (digodoc_html_dir // filename) contents;
  ()

let encode = HTML.encode

open EzFile.OP
open HTML.TYPES

let check_html ~file xml =
  let rec iter =  function
    | PCData _ -> ()
    | Element (tagName, attributes, childNodes) ->
        List.iter iter childNodes;
        match tagName with
        | "a" | "A" ->
            List.iter (function
                  ("href", link) ->
                    begin
                      if link = "" then
                        Printf.eprintf "Empty Link in file %S\n%!" file
                      else
                        let link, _anchor = EzString.cut_at link '#' in
                        if link <> "" &&
                           not (EzString.starts_with link ~prefix:"http") then
                          let dir = Filename.dirname file in
                          let linked_file = dir // link in
                          if not (Sys.file_exists linked_file) then
                            Printf.eprintf "File %s: link %s is dangling\n  %s does not exist\n%!"
                              file link linked_file

                    end
                | _ -> ()) attributes
        | _ -> ()
  in
  iter xml

let rec add_trailer list =
  match list with
  | Element ("div", [ "id", "trailer" ], _ ) :: _ -> list
  | e :: list -> e :: add_trailer list
  | [] ->
      HTML.CONS.[
        div ~a:[ "id", "trailer" ]
          [ hr ;
            p [ s "Generated using ";
                a ~a:[
                  "href", "https://github.com/OCamlPro/digodoc";
                  "target", "digodoc";
                ]
                  [ s "digodoc" ];
                s " at ";
                a ~a:[
                  "href", "https://www.ocamlpro.com";
                  "target", "ocamlpro";
                ]
                  [ s "OCamlPro" ];
              ]
          ]
      ]

let insert_trailer xml =
  let rec iter xml =
    match xml with
    | PCData _ -> xml
    | Element ("div", [ "class", "content" ], childNodes) ->
        Element ("div", [ "class", "content" ], add_trailer childNodes)
    | Element (tagName, attributes, childNodes) ->
        Element ( tagName, attributes, List.map iter childNodes )
  in
  iter xml

let iter_html ?(check_links=false) ?(add_trailer=false) dir =
  Printf.eprintf "Scanning html files...\n%!";
  assert ( check_links ||  add_trailer );
  (*  EzFile.make_select *)
  EzFile.make_select EzFile.iter_dir ~deep:true ~glob:"*.html"
    ~f:(fun path ->
        let file = dir // path in
        match
          match HTML.parse_file file with
          | exception HTML.Error error ->
              Printf.eprintf "%s: invalid html (%s)\n%!"
                file (HTML.string_of_error error);
              None
          | xml -> Some xml
        with
        | None -> ()
        | Some xml ->
            if check_links then check_html ~file xml;
            if add_trailer then
              EzFile.write_file file
                ( "<!DOCTYPE html>\n" ^ HTML.to_string ( insert_trailer xml ) )
      ) dir;
  Printf.eprintf "Scan finished.\n%!"

let add_header_footer () =
  let open Htmlize in 
  Printf.eprintf "Adding header and footer...\n%!";
  let html_dir = digodoc_html_dir in
  let script = {|<script defer="defer" 
                      type="application/javascript" 
                      src="${root-html}headerFooter.js">
                </script>|} in

  EzFile.make_select EzFile.iter_dir ~deep:true ~glob:"*.html"
    ~f:(fun path ->
        if EzString.starts_with ~prefix:"ENTRY" (EzFile.basename path) 
        then ()
        else begin
          let file = html_dir // path in
          let brace () var = 
            match var with
            | "root-html" ->
              let dirname = EzFile.dirname path in 
              let path_list = 
                if String.contains path '/'   
                then 
                  String.split_on_char '/' dirname 
                else []
              in
              let s =
                String.concat "/"
                  (List.map (fun _s -> "..") 
                    path_list)
              in
              if s = "" then s else s ^ "/" 
            | _ -> 
              Printf.kprintf failwith "Unknown var %S" var
          in
        
          let html = EzFile.read_file file 
          and header = Ez_subst.V1.EZ_SUBST.string (Files.body_header ()) ~brace ~ctxt:()
          and footer = Files.body_trailer ()
          and script = Ez_subst.V1.EZ_SUBST.string script ~brace ~ctxt:() in

          let html' = Patchtml.edit_html ~header ~footer ~script html in

          EzFile.remove file;

          EzFile.write_file file html'
        end
      ) html_dir


let write_file file ~content =
  EzFile.write_file file (HTML.check content)

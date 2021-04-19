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

let about_page = 
    Printf.sprintf
        {|<!DOCTYPE html>
        <html xmlns="http://www.w3.org/1999/xhtml">
        <head>
            <title>About</title>
            <link rel="stylesheet" href="_odoc-theme/odoc.css"/>
            <script type="text/javascript" src="search.js" charset="utf-8"></script>
            <meta charset="utf-8"/>
            <meta name="generator" content="digodoc 0.1"/>
            <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
            <script src="highlight.pack.js"></script>
            <script>hljs.initHighlightingOnLoad();</script>
            <script defer="defer" type="application/javascript" src="headerFooter.js"></script>
        </head>
        <body>
            %s 
            <div class="content">
                %s
            </div>
            %s
        </body>
        </html>
        |}
        (Html.file_content "header.html")
        (Html.file_content "about.html")
        (Html.file_content "footer.html")

(* Emulation of doc generation. To use only to check html style / scripts *)
let generate () =
    if EzFile.exists "examples" then 
        EzFile.remove_dir ~all:true "examples";
    EzFile.make_dir ~p:true "examples/html";
    EzFile.make_dir ~p:true "examples/sources";

    (* page example for docs: about.html *)
    Process.call [|"rsync"; "-auv"; "html/.";  "examples/html/." |];
    let brace () var =
        match var with
        | "header_link" -> {| | <a href="#header">To the top</a>|}
        | _ -> ""
    in
    let about_html = Ez_subst.V1.EZ_SUBST.string about_page ~brace ~ctxt:() in
    EzFile.write_file "examples/html/about.html" about_html;

    (* pages examples for sources: config folder *)
    Htmlize.Main.htmlize "examples/sources/" ["config/"];

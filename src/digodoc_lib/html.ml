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

(*
open EzCompat
open Types
*)
open EzFile.OP


let digodoc_html_dir = Globals.digodoc_dir // "html"

(* generate page _digodoc/html/${filename} *)
let generate_page ~filename ~title f =

  let bb = Buffer.create 10000 in
  Printf.bprintf bb {|<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
 <head>
  <title>%s</title>
  <link rel="stylesheet" href="_odoc-theme/odoc.css"/>
  <meta charset="utf-8"/>
  <meta name="generator" content="digodoc 0.1"/>
  <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
  <script src="highlight.pack.js"></script>
  <script>hljs.initHighlightingOnLoad();</script>
</head>
|} title;
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

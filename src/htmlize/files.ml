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

/*
.file-icon {
  width: 20px;
  max-width: 20px;
}

.file-name {
  width: 500px;
}

.file-kind {
  width: 20px;
}
*/

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
    margin-right: 30px;
    margin-left: 30px;
}

.border {
  border-bottom-right-radius: 6px;
  border-bottom-left-radius: 6px;
  box-sizing: border-box;
  padding: 10px;
  margin: -1px -1px 0;
  border: 1px solid #e1e4e8;
  border-top-left-radius: 6px;
  border-top-right-radius: 6px;
}

.position-relative {
  position: relative;
}

.wrap-x {
  overflow-x: auto;
  overflow-y: hidden;
  overflow-wrap: break-word;
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

.sp-k { color: rgb(220,3,3); }
.sp-c { color: rgb(100, 120, 140); }
.sp-s { color: rgb(3, 50, 100); }
.sp-n { color: rgb(3, 100, 200); }
.sp-m { color: #005cc5; }
.sp-l { color: #22863a; }
.sp-f { color: #6f42c1; }
.sp-a { color: #e36209; }
.sp-typ { color: #32a190; }
.sp-syn { color: #ff6666; }

.line {
  font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;
  font-size: 12px;
  line-height: 15px;
  vertical-align: top;
}

.line-num {
    min-width: 30px;
    padding-right: 15px;
    color: rgba(27,31,35,0.3);
    text-align: right;
    white-space: nowrap;
    cursor: pointer;
    -webkit-user-select: none;
    -moz-user-select: none;
    -ms-user-select: none;
    user-select: none;
}

.line-code {
  white-space: pre;
  padding-right: 10px;
  padding-left: 5px;
  position: relative;
  vertical-align: top;
}

|}
let script_js = {||}

let html_header = {|<!DOCTYPE html>
<html lang="en">
 <head>
   <meta charset="utf-8">
   <title>${title}</title>
   <link rel="stylesheet" href="${root}_static/style.css" />
   <script defer="defer" type="application/javascript" src="${root}_static/script.js"></script>
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
  <div class="file-info border">${title-info}</div>
  <div class="content-info border">${content-info}</div>
  <div class="content-div border">${content}</div>
 </div>
</div>
|}

let svg_directory = {|
<svg aria-label="Directory" class="octicon octicon-file-directory text-color-icon-directory" height="16" viewBox="0 0 16 16" version="1.1" width="16" role="img"><path fill-rule="evenodd" d="M1.75 1A1.75 1.75 0 000 2.75v10.5C0 14.216.784 15 1.75 15h12.5A1.75 1.75 0 0016 13.25v-8.5A1.75 1.75 0 0014.25 3h-6.5a.25.25 0 01-.2-.1l-.9-1.2c-.33-.44-.85-.7-1.4-.7h-3.5z"></path></svg>
|}

let svg_file = {|
<svg aria-label="File" class="octicon octicon-file text-gray-light" height="16" viewBox="0 0 16 16" version="1.1" width="16" role="img"><path fill-rule="evenodd" d="M3.75 1.5a.25.25 0 00-.25.25v11.5c0 .138.112.25.25.25h8.5a.25.25 0 00.25-.25V6H9.75A1.75 1.75 0 018 4.25V1.5H3.75zm5.75.56v2.19c0 .138.112.25.25.25h2.19L9.5 2.06zM2 1.75C2 .784 2.784 0 3.75 0h5.086c.464 0 .909.184 1.237.513l3.414 3.414c.329.328.513.773.513 1.237v8.086A1.75 1.75 0 0112.25 15h-8.5A1.75 1.75 0 012 13.25V1.75z"></path></svg>
|}

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

/************ Links *****************/

/* Style the links inside the navigation bar */

.content a {
    color: #0366d6;
    text-decoration: none;
}

.topnav a {
  float: left;
  color: #f2f2f2;
  text-align: center;
  padding: 14px 16px;
  text-decoration: none;
  font-size: 17px;
}

.file-info a:hover{
  text-decoration: underline;
}

.file-link:hover{
  background-color: #f1f1f1;
  color:black;
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

/**************** Divs *****************/

.file{
  background-color: inherit;
  min-height: 30px;
  border: 0px;
}

.content-div, .round-border{
  border-bottom-right-radius: 10px;
  border-bottom-left-radius: 10px;
}

.file.top-border {
  border-top: 1px solid #e1e4e8;
}

.content {
    font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace !important;
    max-width: 1280px;
}

.border {
  box-sizing: border-box;
  margin: -1px -1px 0;
  border: 1px solid #e1e4e8;
}

.padding {
  padding: 10px;
}

.position-relative {
  position: relative;
}

.wrap-x {
  overflow-x: auto;
  overflow-y: hidden;
  overflow-wrap: break-word;
}

.file-info {
  margin-top: 20px;
  font-size: 14px;
  background-color: #f6f8fa;
  border-bottom-right-radius: 10px;
  border-bottom-left-radius: 10px;
  border-top-left-radius: 10px;
  border-top-right-radius: 10px;
}

.content-info {
  margin-top: 20px;
  background-color: #f6f8fa;
  border-top-left-radius: 10px;
  border-top-right-radius: 10px;
}

/* Add a black background color to the top navigation */
.topnav {
    background-color: #333;
    overflow: hidden;
}

/************** Tables, rows, cells ***************/

.file-tab{
  width: 100%;
} 

.file-name{
  padding-left:15px;
}

.file-icon {
  width: 3%;
  min-width: 25px;
}

.file-kind {
  font-size:12px;
  min-width: 120px;
  width: 20%;
}

/* Right-aligned section inside the top navigation */
.topnav-right {
  float: right;
}

.content-table {
  border-color: grey;
}


.line {
  font-family: SFMono-Regular,Consolas,Liberation Mono,Menlo,monospace;
  font-size: 12px;
  line-height: 15px;
  vertical-align: top;
}

.line-num {
    min-width: 20px;
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


/******** Spans ***********/

.sp-k { color: rgb(220,3,3); }
.sp-c { color: rgb(100, 120, 140); }
.sp-s { color: rgb(3, 50, 100); font-style: italic }
.sp-n { color: rgb(3, 100, 200); }
.sp-m { color: #005cc5; }
.sp-l { color: #22863a; }
.sp-f { color: #6f42c1; }
.sp-a { color: #e36209; }
.sp-typ { color: #32a190; }
.sp-syn { color: #ff6666; }
.sp-ch { color: #0000ff; }

.separator {
  font-size: 16px;
  color: #dcdcdc;
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
  <div class="file-info border padding">${title-info}</div>
  <div class="content-info border padding">${content-info}</div>
  <div class="content-div border">${content}</div>
 </div>
</div>
|}

let svg_directory = {|
<svg aria-label="Directory" style="color:#79b8ff !important; fill:currentColor;  display: block; margin:auto" height="16" viewBox="0 0 16 16" version="1.1" width="16" role="img"><path fill-rule="evenodd" d="M1.75 1A1.75 1.75 0 000 2.75v10.5C0 14.216.784 15 1.75 15h12.5A1.75 1.75 0 0016 13.25v-8.5A1.75 1.75 0 0014.25 3h-6.5a.25.25 0 01-.2-.1l-.9-1.2c-.33-.44-.85-.7-1.4-.7h-3.5z"></path></svg>
|}

let svg_file = {|
<svg aria-label="File" style="color:#959da5 !important; fill:currentColor; display: block; margin:auto" height="16" viewBox="0 0 16 16" version="1.1" width="16" role="img"><path fill-rule="evenodd" d="M3.75 1.5a.25.25 0 00-.25.25v11.5c0 .138.112.25.25.25h8.5a.25.25 0 00.25-.25V6H9.75A1.75 1.75 0 018 4.25V1.5H3.75zm5.75.56v2.19c0 .138.112.25.25.25h2.19L9.5 2.06zM2 1.75C2 .784 2.784 0 3.75 0h5.086c.464 0 .909.184 1.237.513l3.414 3.414c.329.328.513.773.513 1.237v8.086A1.75 1.75 0 0112.25 15h-8.5A1.75 1.75 0 012 13.25V1.75z"></path></svg>
|}

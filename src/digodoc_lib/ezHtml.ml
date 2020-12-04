(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let buffer b s =
  let add = Printf.bprintf b in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '&' -> add "&amp;"
    | '<' -> add "&lt;"
    | '>' -> add "&gt;"
    | '\'' -> add "&apos;"
    | '\"' -> add "&quot;"
    | '@' -> add "&commat;"
    | c ->
        Buffer.add_char b c
  done

let string s =
  let b = Buffer.create (String.length s + 11) in
  buffer b s;
  Buffer.contents b

let encode s = string s

let check ?(msg="") content =
  begin match Xml.parse_string content with
    | exception Xml.Error error ->
        Printf.eprintf "Invalid Html%s: %s\n%!" msg
          (Xml.error error);
        Printf.eprintf "<<<\n%s\n>>>\n" content;
    | _ -> ()
  end;
  content

let write_file file ~content =
  EzFile.write_file file (check content)

open EzFile.OP

let check_html ~file xml =
  let open Xml_types in
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

let check_links dir =
  (*  EzFile.make_select *)
  EzFile.make_select EzFile.iter_dir ~deep:true ~glob:"*.html"
    ~f:(fun path ->
        let file = dir // path in
        match Xml.parse_file file with
        | exception Xml.Error error ->
            Printf.eprintf "%s: invalid html (%s)\n%!"
              file (Xml.error error)
        | exception Dtd.Parse_error _error ->
            begin
              match EzFile.read_lines_to_list file with
              | [] -> assert false
              | _ :: lines ->
                  let content = String.concat "\n" lines in
                  match Xml.parse_string content with
                  | exception Xml.Error error ->
                      Printf.eprintf "%s: invalid html (%s)\n%!"
                        file (Xml.error error)
                  | xml -> check_html ~file xml
            end
        | xml -> check_html ~file xml
      ) dir

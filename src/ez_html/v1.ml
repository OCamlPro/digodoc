(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module HTML = struct

  module TYPES = struct

    (* [Element (tag-name, attributes, children)] or [PCData text] *)
    type xml = Xml_types.xml =
      | Element of (string * (string * string) list * xml list)
      | PCData of string

  end

  open TYPES

  module CONS = struct
    type t = ?a:(string * string) list -> TYPES.xml list -> TYPES.xml

    let element tagName ?(a=[]) childNodes =
      Element (tagName, a, childNodes)
    let s s = PCData s
    let div = element "div"
    let p = element "p"
    let a = element "a"
    let hr = element "hr" []
  end

  let encode_to_buffer b s =
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

  let encode s =
    let b = Buffer.create (String.length s + 11) in
    encode_to_buffer b s;
    Buffer.contents b

  let check ?(msg="") content =
    begin match Xml.parse_string content with
      | exception Xml.Error error ->
          Printf.eprintf "Invalid Html%s: %s\n%!" msg
            (Xml.error error);
          Printf.eprintf "<<<\n%s\n>>>\n" content;
      | _ -> ()
    end;
    content

  type error =
    | XmlError of Xml.error
    | DtdParseError of Dtd.parse_error

  exception Error of error

  let parse f file =
    try f file with
    | Xml.Error error -> raise (Error (XmlError error))
    | Dtd.Parse_error error -> raise (Error (DtdParseError error))

  let parse_file file = parse Xml.parse_file file
  let parse_string s = parse Xml.parse_string s
  let to_string s = Xml.to_string s


  let string_of_error = function
    | XmlError error -> Xml.error error
    | DtdParseError error -> Dtd.parse_error error

end

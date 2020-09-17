(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2011-2020 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

open Types
open Lexer

let string_of_token = function
  | STRING s -> Printf.sprintf "STRING %S" s
  | IDENT  s -> Printf.sprintf "IDENT %S" s
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | EQUAL -> "EQUAL"
  | PLUSEQUAL -> "PLUSEQUAL"
  | MINUS -> "MINUS"
  | EOF -> "EOF"

let rec tokens_of_file verbose filename =
  try
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let tokens = ref [] in
  let rec iter lexbuf =
    let token = Lexer.token lexbuf in
    if verbose then
      Printf.eprintf "[%s]\n%!" (string_of_token token);
    match token with
    | EQUAL
    | STRING  _
    | IDENT  _
    | LPAREN
    | RPAREN
    | MINUS
    | PLUSEQUAL
      ->
      tokens := token :: !tokens;
        iter lexbuf
    | EOF -> ()

  in
  begin try
  iter lexbuf;
    with Lexer.Error ->
      let loc = Lexing.lexeme_start lexbuf in
      Printf.eprintf "Syntax error at pos %d\n%!"
        loc;
      if not verbose then
        ignore (tokens_of_file true filename : Lexer.token list)
      else
        raise Lexer.Error
  end;
  close_in ic;
  List.rev !tokens
  with e ->
    Printf.eprintf "Exception %S while parsing %S\n%!" (Printexc.to_string e) filename;
    raise e

let create p_parent =
  {
    p_parent;
    p_packages = [];
    p_variables = StringMap.empty;
  }


let close_package p =
  StringMap.iter (fun _name v ->
      if v.var_assigns <> [] then
        v.var_assigns <- List.rev v.var_assigns;
      if v.var_additions <> [] then
        v.var_additions <- List.rev v.var_additions;
    ) p.p_variables;
  ()

let get_variable p var_name =
  try
    StringMap.find var_name p.p_variables
  with Not_found ->
    let v = {
      var_name;
      var_additions = [];
      var_assigns = [];
    } in
    p.p_variables <- StringMap.add var_name v p.p_variables;
    v

let parse_file filename =
  let tokens = tokens_of_file false filename in

  let rec iter p path tokens =
    match tokens with
      [] ->
      begin
        match path with
          [] ->
          close_package p
        | (name, _) :: _ ->
          failwith (
            Printf.sprintf "missing right parenthesis for package %s" name)
      end

    | IDENT name :: EQUAL :: STRING str :: tokens ->
      (*      Printf.eprintf "IDENT[%s]\n%!" name; *)
      let v = get_variable p name in
      v.var_assigns <- ([], str) :: v.var_assigns;
      iter p path tokens

    | IDENT name :: PLUSEQUAL :: STRING str :: tokens ->
      let v = get_variable p name in
      v.var_additions <- ([], str) :: v.var_additions;
      iter p path tokens

    | IDENT name :: LPAREN :: tokens ->
      (*      Printf.eprintf "IDENT()[%s]\n%!" name; *)
      iter_precond p path name [] tokens

    | IDENT "package" :: STRING package_name :: LPAREN :: tokens ->
      let new_p = create (Some p) in
      p.p_packages <- (package_name, new_p) :: p.p_packages;
      iter new_p ( (package_name,p) :: path) tokens

    | RPAREN :: tokens ->
      begin
        match path with
        | (_name, old_p) :: path ->
          close_package p;
          iter old_p path tokens
        | [] -> failwith "Right parenthesis without matching left"
      end
    | _ ->
      print_remaining "iter" tokens

  and print_remaining msg tokens =
    Printf.eprintf "%s: Don't know what to do with:\n%!" msg;
    begin try
        let num = ref 0 in
        List.iter (fun token ->
            if !num = 3 then begin
              Printf.eprintf "   ...\n%!";
              raise Exit
            end;
            Printf.eprintf "  %s\n%!" (string_of_token token);
            incr num;
          ) tokens;
      with Exit -> ()
    end;
    failwith "Unexpected tokens"

  and iter_precond (p : t) path name preconds tokens =
    match tokens with
    | RPAREN ::EQUAL :: STRING str :: tokens ->
      let v = get_variable p name in
      v.var_assigns <- (preconds, str) :: v.var_assigns;
      iter p path tokens

    | RPAREN ::PLUSEQUAL :: STRING str :: tokens ->
      let v = get_variable p name in
      v.var_additions <- (preconds, str) :: v.var_additions;
      iter p path tokens

    | IDENT ident :: tokens ->
      iter_precond p path name ((ident, true) :: preconds) tokens

    | MINUS :: IDENT ident :: tokens ->
      iter_precond p path name ((ident, false) :: preconds) tokens

    | _ ->
      print_remaining "iter_precond" tokens

  in
  let p = create None in
  iter p [] tokens;
  p

let name_of_META filename =
  let basename = Filename.basename filename in
  let long_name =
    if basename = "META" then
      Filename.basename (Filename.dirname filename)
    else
      if EzString.starts_with basename ~prefix:"META." then
        String.sub basename 5 (String.length basename - 5)
      else
        failwith (Printf.sprintf
                    "MetaParser.name_of_META: incorrect filename %S"
                    filename)
  in
  let (name, _version) = EzString.cut_at long_name '.' in
  name

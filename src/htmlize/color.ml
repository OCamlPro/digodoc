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

open EzCompat


type color =
  | TEXT
  | KEYWORD
  | COMMENT
  | STRING
  | NUMBER
  | CHAR
  | MODULE
  | LABEL
  | FUNCTION
  | ARGUMENT
  | TYPE
  | SYNTAX

module OCAML = struct

  let color_of_token = function
    | Approx_tokens.AND
    | AS
    | ASSERT
    | BEGIN
    | CLASS
    | CONSTRAINT
    | DO
    | DONE
    | DOWNTO
    | ELSE
    | END
    | EXCEPTION
    | EXTERNAL
    | FOR
    | FUN
    | FUNCTION
    | FUNCTOR
    | IF
    | IN
    | INCLUDE
    | INHERIT
    | INITIALIZER
    | LAZY
    | LET
    | MATCH
    | METHOD
    | MODULE
    | MUTABLE
    | NEW
    | NOT
    | OBJECT
    | OF
    | OPEN
    | OR
    | PRIVATE
    | REC
    | REF
    | SIG
    | STRUCT
    | THEN
    | TO
    | TRY
    | TYPE
    | VAL
    | VIRTUAL
    | WHEN
    | WHILE
    | WITH

      -> KEYWORD

    | COMMENT_OPEN_EOL
    | COMMENT_OPEN
    | COMMENT_OPEN_CLOSE
    | COMMENT_VERB_OPEN
    | COMMENT_CODE_OPEN
    | COMMENT_CONTENT
    | COMMENT_CLOSE
    | COMMENT_VERB_CLOSE
    | COMMENT_CODE_CLOSE

    | LINE_DIRECTIVE _
      -> COMMENT

    
    | BARRBRACKET
    | COLON
    | COLONCOLON
    | DOT
    | DOTDOT
    | EOF
    | GREATERRBRACE
    | GREATERRBRACKET

    | LBRACE
    | LBRACELESS
    | LBRACKET
    | LBRACKETBAR
    | LBRACKETLESS
    | LBRACKETGREATER
    | LBRACKETPERCENT
    | LBRACKETPERCENTPERCENT
    | LBRACKETAT
    | LBRACKETATAT
    | LBRACKETATATAT
    | LIDENT _
    | LPAREN
    | MINUSGREATER
    | PREFIXOP _
    | QUOTE
    | RBRACE
    | RBRACKET
    | RPAREN
    | SEMI
    | SEMISEMI

    | ESCAPED_EOL
    | EOL
    | SPACES

    | ILLEGAL_CHAR _
    | COMMA

    | PPX_QUOTATION_OPEN
    | PPX_QUOTATION_CONTENT
    | PPX_QUOTATION_CLOSE

    | P4_QUOTATION_OPEN
    | P4_QUOTATION_CONTENT
    | P4_QUOTATION_CLOSE

      -> TEXT

    | STRING_OPEN
    | STRING_CONTENT
    | STRING_CLOSE
      -> STRING

    | TRUE
    | FALSE
    | FLOAT _
    | INT _
    | INT32 _
    | INT64 _
    | NATIVEINT _
      -> NUMBER

    | UIDENT _
    | UNIT
    | EMPTYLIST
        -> MODULE

    | Approx_tokens.LABEL _ -> LABEL

    | LFUNCTION _ | CONSTRUCTOR _ -> FUNCTION
    | LARGUMENT _ -> ARGUMENT
    | LTYPE _ | TYPEVAR -> TYPE

    | BANG
    | AMPERAMPER
    | AMPERSAND
    | BACKQUOTE
    | BAR
    | BARBAR
    | COLONEQUAL
    | COLONGREATER
    | EQUAL
    | GREATER
    | INFIXOP0 _
    | LESS
    | LESSMINUS
    | MINUS
    | MINUSDOT
    | OPTLABEL _
    | PLUS
    | PLUSDOT
    | QUESTION
    | QUESTIONQUESTION
    | SHARP
    | STAR
    | TILDE
    | UNDERSCORE
    | INFIXOP1 _
    | INFIXOP2 _
    | INFIXOP3 _
    | INFIXOP4 _
      -> SYNTAX

    | CHAR _ -> CHAR

  let file content =
    let len = String.length content in
    let colors = Array.make len TEXT in

    let tokens = Approx_lexer.tokens_of_string content in

    (*List.iter (fun tok -> Printf.printf "%s " (Approx_tokens.string_of_tok tok)) (List.map fst tokens);*)

    let tokens = Transformer.transform tokens in

    List.iter (fun (token, ( (lex_start, lex_end), _, _)) ->
        let lex_start = lex_start.Lexing.pos_cnum in
        let lex_end = lex_end.Lexing.pos_cnum in
        let color = color_of_token token in
        for i = lex_start to lex_end-1 do
          colors.(i) <- color
        done;
      ) tokens;
colors
end


let file filename content =
  let colors =
    let len = String.length content in
    let basename = Filename.basename filename in
    let _, ext = EzString.rcut_at basename '.' in
    let ext = String.lowercase ext in
    match ext with
    | "ml" | "mli" | "mll" | "mly" -> OCAML.file content
    | _ -> Array.make len TEXT
  in

  let lines = EzString.split content '\n' in

  let rec get_colors pos line i line_len =
    if line_len = 0 then
      []
    else
      let color = colors.(pos) in
      get_next_colors color (pos+1) line i (i+1) (line_len-1)

  and get_next_colors color pos line i0 i line_len =
    if line_len = 0 then
      [ color, String.sub line i0 (i-i0) ]
    else
      let color2 = colors.(pos) in
      if color = color2 then
        get_next_colors color (pos+1) line i0 (i+1) (line_len-1)
      else
        ( color, String.sub line i0 (i-i0) ) ::
        get_next_colors color2 (pos+1) line i (i+1) (line_len-1)
  in

  let rec iter linenum pos lines rev =
    (*    Printf.eprintf "linenum=%d\n%!" linenum; *)
    match lines with
    | [] -> List.rev rev
    | line :: lines ->
        let line_len = String.length line in
        let colors = get_colors pos line 0 line_len in
        let rev = (linenum, colors) :: rev in
        iter (linenum+1) (pos+line_len+1) lines rev
  in
  iter 1 0 lines []

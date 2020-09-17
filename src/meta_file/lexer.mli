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

type token =
| STRING of string
| IDENT of string
| LPAREN
| RPAREN
| PLUSEQUAL
| EQUAL
| MINUS
| EOF

exception Error

val token : Lexing.lexbuf -> token

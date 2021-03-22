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

open Approx_tokens

(* Research of function's name. Functions name are considered in those cases:
   1) After the tokens 'let' and 'lident' there should be at least one token that is not 'equal' or 'spaces'*) 



let transform_let tokens  =
    let in_let = ref false 
    and first_ident = ref true 
    and is_function = ref false
    and is_tuple = ref false
    and is_type = ref false
    and is_optional = ref false
    and len = Array.length tokens in
    for i = 0 to len-1 do
        if !in_let then
            match tokens.(i) with
            | LIDENT name when !first_ident ->
                first_ident := false;
                let j = ref (i+1) in
                while (tokens.(!j) <> EQUAL) || !is_optional do
                    begin 
                        match tokens.(!j) with
                        | COMMA -> is_tuple := true
                        | COLON -> is_type := true
                        | QUESTION -> is_optional := true
                        | RPAREN -> is_type :=false; is_optional := false
                        | LIDENT name when !is_type -> 
                            tokens.(!j) <- LTYPE name
                        | UIDENT name when !is_type -> tokens.(!j) <- LTYPE name
                        | SPACES -> ()
                        | _ when not !is_tuple ->
                            is_function:=true
                        | _ -> ();
                    end;
                    j:=!j+1
                done;
                if !is_function then tokens.(i) <- LFUNCTION name
            | LIDENT name when !is_function ->
                tokens.(i) <- LARGUMENT name
            | QUESTION -> is_optional := true
            | RPAREN -> is_optional := false
            | EQUAL when not !is_optional ->
                in_let:= false;
                is_function:=false;
                is_tuple:=false;
                is_type:=false;
                first_ident:=true
            | _ -> ()
        else
            match tokens.(i) with
            | LET -> in_let:=true
            | _ -> ()
    done;
    tokens

let transform tokens =
    let (toks,toks_inf) = List.split tokens in
    let toks = Array.of_list toks in
    let toks = transform_let toks in
    let toks = Array.to_list toks in
    List.combine toks toks_inf
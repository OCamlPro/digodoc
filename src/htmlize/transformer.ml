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
            (*TODO: stack for parentheses, to know the one that closes typed or optional argument*)
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

let transform_fun tokens  =
    let in_fun = ref false 
    and is_type = ref false
    and is_optional = ref false
    and len = Array.length tokens in
    for i = 0 to len-1 do
        if !in_fun then
            match tokens.(i) with
            | LIDENT name when !is_type -> 
                tokens.(i) <- LTYPE name
            | LIDENT name ->
                tokens.(i) <- LARGUMENT name
            | COLON -> is_type := true
            | QUESTION -> is_optional := true
            (*TODO: stack for parentheses, to know the one that closes typed or optional argument*)
            | RPAREN -> is_type :=false; is_optional := false
            | MINUSGREATER ->
                in_fun:= false;
                is_type:=false;
            | _ -> ()
        else
            match tokens.(i) with
            | FUNCTION | FUN -> in_fun:=true
            | _ -> ()
    done;
    tokens

let transform_match tokens  =
    let in_match = ref false 
    and is_type = ref false
    and len = Array.length tokens in
    for i = 0 to len-1 do
        if !in_match then
            match tokens.(i) with
            | LIDENT name when !is_type -> 
                tokens.(i) <- LTYPE name
            | LIDENT name ->
                tokens.(i) <- LARGUMENT name
            | COLON -> is_type := true
            (*TODO: stack for parentheses, to know the one that closes typed argument*)
            | RPAREN -> is_type :=false;
            | MINUSGREATER ->
                in_match:= false;
                is_type:=false;
            | _ -> ()
        else
            match tokens.(i) with
            | BAR | WITH -> in_match:=true
            | _ -> ()
    done;
    tokens

let transform_cons tokens = 
    let is_module_name tok =
        match tok with
        | UIDENT _ -> true 
        | _ -> false
    in
    let in_mod_dec = ref false
    and len = Array.length tokens 
    and i = ref 0 in
    while !i < len do
        begin
            if !in_mod_dec then begin
                (* Skip open/include/module expression *)
                while not (is_module_name tokens.(!i))  do
                    i:= !i+1
                done; 
                while not (tokens.(!i) = SPACES || tokens.(!i) = EOL ) do
                    i:= !i+1
                done;
                in_mod_dec := false
            end
            else 
                match tokens.(!i) with
                | OPEN | INCLUDE | MODULE -> in_mod_dec := true
                | UIDENT _ when tokens.(!i+1) = DOT -> ()
                | UIDENT s -> tokens.(!i) <- CONSTRUCTOR s
                | _ -> ();
        end;
        i:=!i+1
    done;
    tokens

let transform tokens =
    let (toks,toks_inf) = List.split tokens in
    let toks = Array.of_list toks in
    let toks = transform_let toks in
    let toks = transform_fun toks in
    let toks = transform_match toks in
    let toks = transform_cons toks in
    let toks = Array.to_list toks in
    List.combine toks toks_inf
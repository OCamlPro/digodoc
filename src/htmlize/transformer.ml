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

let is_module_name tok =
    match tok with
    | UIDENT _ -> true 
    | _ -> false

let is_ident_name tok =
    match tok with
    | LIDENT _ -> true 
    | _ -> false


let transform_let tokens  =
    let in_let = ref false 
    and first_ident = ref true 
    and is_function = ref false
    and is_tuple = ref false
    and is_type = ref false
    and is_optional = ref false
    and len = Array.length tokens
    and fd = EzFile.open_out "pes.txt" in
    for i = 0 to len-1 do
        Printf.fprintf fd "%d %s\n" i (string_of_tok tokens.(i));
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
            | OPEN -> in_let := false
            | _ -> ()
        else
            match tokens.(i) with
            | LET when tokens.(i+1) = SPACES &&  
                        is_ident_name tokens.(i+2) -> in_let:=true
            | LET when tokens.(i+2) = REC -> in_let:=true
            | AND -> in_let:=true
            | _ -> ()
    done;
    close_out fd;
    EzFile.remove "pes.txt";
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
    and in_record = ref false
    and in_type = ref false
    and len = Array.length tokens in
    for i = 0 to len-1 do
        if !in_match && not !in_type then
            match tokens.(i) with
            | LIDENT name when !is_type -> 
                tokens.(i) <- LTYPE name
            | LIDENT name ->
                tokens.(i) <- LARGUMENT name
            | COLON -> is_type := true
            (*TODO: stack for parentheses, to know the one that closes typed argument*)
            | RPAREN -> is_type :=false;
            | MINUSGREATER | WHEN ->
                in_match:= false;
                is_type:=false;
            | _ -> ()
        else
            match tokens.(i) with
            | BAR  -> in_match:=true
            | WITH when not !in_record -> in_match:=true 
            | LBRACE -> in_record:=true
            | RBRACE -> in_record:=false
            | TYPE -> in_type:=true; 
            | LET -> in_type:=false; in_match:=false
            | _ -> ()
    done;
    tokens

let transform_cons tokens = 
    let in_mod_dec = ref false
    and len = Array.length tokens 
    and i = ref 0 
    and in_arguments = ref false
    and fd = EzFile.open_out "pes.txt" in
    while !i < len do
        Printf.fprintf fd "%d %s\n" !i (string_of_tok tokens.(!i));
        begin
            if !in_mod_dec then begin
                (* Skip open/include/module expression *)
                while not (is_module_name tokens.(!i)) &&
                      not (is_ident_name tokens.(!i)) &&
                      tokens.(!i) <> STRUCT do
                    i:= !i+1
                done; 
                while not (tokens.(!i) = SPACES || tokens.(!i) = EOL ) do
                    i:= !i+1
                done;
                in_mod_dec := false
            end
            else 
                match tokens.(!i) with
                | OPEN | INCLUDE | MODULE when not !in_arguments -> in_mod_dec := true
                | UIDENT _ when tokens.(!i+1) = DOT -> ()
                | UIDENT s -> tokens.(!i) <- CONSTRUCTOR s
                | LET |FUNCTION | FUN ->  in_arguments := true
                | MINUSGREATER | EQUAL ->  in_arguments := false
                | _ -> ();
        end;
        i:=!i+1
    done;
    close_out fd;
    EzFile.remove "pes.txt";
    tokens   

let transform_type tokens  =
    let is_type = ref false
    and in_val = ref false
    and in_type = ref false
    and in_record = ref false
    and len = Array.length tokens
    and fd = EzFile.open_out "pes.txt" in
    for i = 0 to len-1 do
        Printf.fprintf fd "%d %s\n" i (string_of_tok tokens.(i));
        if !in_type then
            match tokens.(i) with
            | LIDENT name when not !in_record || !is_type -> 
                tokens.(i) <- LTYPE name
            | COLON -> is_type := true
            | SEMI -> is_type :=false
            (*TODO: stack for braces, to know the one that closes record*)
            | LBRACE -> in_record := true
            | RBRACE -> in_record := false
            | LET | MODULE | OPEN | INCLUDE 
            | EXTERNAL | CLASS -> 
                in_type:=false;
                is_type:=false
            | VAL ->
                in_type:=false;
                is_type:=false;
                in_val:=true
            | _ -> ()
        else if !in_val then
            match tokens.(i) with 
            | COLON -> is_type := true
            | LIDENT name when !is_type ->
                tokens.(i) <- LTYPE name
            | LET | MODULE | OPEN | INCLUDE 
            | EXTERNAL | CLASS ->
                is_type:=false; 
                in_val:=false
            | TYPE | EXCEPTION->
                in_type :=true;
                is_type:=false;
                in_val:=false               
            | VAL -> is_type:=false 
            | _ -> ()
        else 
            match tokens.(i) with
            | TYPE | EXCEPTION-> in_type:=true;
            | VAL -> in_val:=true;
            | _ -> ()
    done;
    close_out fd;
    EzFile.remove "pes.txt";
    tokens


let transform tokens =
    let rec safe_split l accX accY =
        match l with
        | [] -> List.rev accX, List.rev accY
        | (x,y)::ll -> safe_split ll (x::accX) (y::accY)
    in
    let rec safe_combine l1 l2 acc =
        match l1,l2 with
        | [],[] -> List.rev acc
        | x::xs,y::ys -> safe_combine xs ys ((x,y)::acc)
        | _ -> failwith "Invalid argument" 
    in
    let (toks,toks_inf) = safe_split tokens [] [] in
    let toks = Array.of_list toks in
    Printf.printf "SIZE OF LIST=%d\n" (Array.length toks);
    Array.iter (fun tok -> Printf.printf "%s " (Approx_tokens.string_of_tok tok))  toks;
    let toks' = begin Printf.printf "\nTRANSFORM LET\n"; transform_let toks end in 
         Array.iter (fun tok -> Printf.printf "%s " (Approx_tokens.string_of_tok tok))  toks';

     let toks' = begin Printf.printf "TRANSFORM FUN\n";transform_fun toks' end in
              Array.iter (fun tok -> Printf.printf "%s " (Approx_tokens.string_of_tok tok))  toks';

     let toks' = begin Printf.printf "TRANSFORM MATCH\n";transform_match toks' end in 
              Array.iter (fun tok -> Printf.printf "%s " (Approx_tokens.string_of_tok tok))  toks';

     let toks' = begin Printf.printf "TRANSFORM CONS\n";transform_cons toks' end in 
              Array.iter (fun tok -> Printf.printf "%s " (Approx_tokens.string_of_tok tok))  toks';

     let toks' =  begin Printf.printf "TRANSFORM TYPE\n";transform_type toks' end 
    in
        let toks' = Array.to_list toks' in
        let x = safe_combine toks' toks_inf [] in
        Printf.printf "BASRTTTT\n"; x

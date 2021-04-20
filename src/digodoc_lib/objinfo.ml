(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2020 OCamlPro SAS & Origin Labs SAS                     *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

open EzCompat
open Type
open EzFile.OP

let read state filename =
  let filename = state.opam_switch_prefix // filename in
  if Sys.file_exists filename then
    let lines = Process.call_stdout [| "ocamlobjinfo" ;
                                "-no-approx";
                                "-no-code" ;
                                filename
                             |] in
    let rec iter0 lines =
      match lines with
      | [] -> []
      | line :: lines ->
          let field_name, field_value = EzString.cut_at line ':' in
          let field_value = String.trim field_value in
          match field_name with
          | "Unit name" | "Name" ->
              let unit = {
                unit_name = field_value ;
                unit_implementation = None ;
                unit_import_cmis = StringMap.empty ;
                unit_import_cmxs = StringMap.empty ;
              }
              in
              iter1 unit lines
          | _ -> iter0 lines

    and iter1 unit lines =
      match lines with
      | [] -> [ unit ]
      | line :: lines ->
          let field_name, field_value = EzString.cut_at line ':' in
          let field_value = String.trim field_value in
          match field_name with
          | "Interfaces imported" ->
              iter2 unit (fun unit name crc ->
                  { unit with
                    unit_import_cmis =
                      StringMap.add name crc unit.unit_import_cmis}) lines
          | "Implementations imported" ->
              iter2 unit (fun unit name crc ->
                  { unit with
                    unit_import_cmxs =
                      StringMap.add name crc unit.unit_import_cmxs}) lines
          | "Unit name" | "Name" ->
              let new_unit = {
                unit_name = field_value ;
                unit_implementation = None ;
                unit_import_cmis = StringMap.empty ;
                unit_import_cmxs = StringMap.empty ;
              }
              in
              unit :: (iter1 new_unit lines)
          | "CRC of implementation" ->
              iter1 { unit with unit_implementation = Some field_value } lines
          | _ -> iter1 unit lines

    and iter2 unit set lines =
      match lines with
      | [] -> [ unit ]
      | line :: new_lines ->
          if line.[0] = '\t' then
            match EzString.split line '\t' with
              [ "" ; crc ; name ] ->
                iter2 (set unit name crc) set new_lines
            | _ ->
                Printf.eprintf "Wrong format: %S\n%!" line;
                iter1 unit lines
          else
            iter1 unit lines
    in
    let units = iter0 ( Array.to_list lines ) in
    begin
      if Filename.check_suffix filename ".cmx" then begin
        match units with
          [] | _ :: _ :: _ -> assert false
        | [ unit ] ->
            match unit.unit_implementation with
            | None -> assert false
            | Some _ -> ()
      end
    end;
    units

  else
    []

let print unit =
  Printf.printf "unit_name: %S\n" unit.unit_name;
  (match unit.unit_implementation with
   | None -> () | Some crc ->
       Printf.printf "   unit_implementation: %S\n" crc);
  StringMap.iter (fun name crc ->
      Printf.printf "    unit_import_cmi: %S -> %S\n" name crc)
    unit.unit_import_cmis;
  StringMap.iter (fun name crc ->
      Printf.printf "    unit_import_cmx: %S -> %S\n" name crc)
    unit.unit_import_cmxs;
  ()

(*
let () =

  let opam_switch_prefix = "." in
  let state = {
    opam_switch_prefix ;
    opam_packages = StringMap.empty ;
    meta_packages = StringMap.empty ;
    ocaml_libs = Hashtbl.create 13 ;
    ocaml_mdls = Hashtbl.create 13 ;
    directories = StringMap.empty ;
  } in

  List.iter print (read state "jsonm/jsonm.cmi");
  List.iter print (read state "jsonm/jsonm.cmxa");
  List.iter print (read state "angstrom/angstrom.cmxa");
  if true then exit 4
*)

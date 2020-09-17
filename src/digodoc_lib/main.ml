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
open EzFile.OP
open OpamParserTypes
open Types

module OPAM = struct

  let create state opam_name opam_files =
    if StringMap.mem opam_name state.opam_packages then
      Printf.kprintf failwith "Duplicated opam package %s" opam_name;
    let p = {
      opam_name ;
      opam_version = None ;
      opam_files ;
      opam_metas = [] ;
      opam_libs = StringMap.empty ;
    } in
    state.opam_packages <- StringMap.add opam_name p state.opam_packages ;
    p

let kind = function
  | String _ -> "string"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Relop _ -> "relop"
  | Prefix_relop _ -> "prefix"
  | Logop _ -> "logop"
  | Pfxop _ -> "pfxop"
  | Ident _ -> "ident"
  | List _ -> "list"
  | Group _ -> "group"
  | Option _ -> "option"
  | Env_binding _ -> "env_binding"


let parse_changes filename =
  let opamfile = OpamParser.file filename in
  let files = ref [] in
  let add_file file info =
    let c = info.[0] in
    let kind =
      match c with
      | 'D' -> Directory
      | 'F' -> File
      | 'L' -> Link
      | _ ->
          Printf.eprintf "[ %c ]%!" c;
          assert false
    in
    files := (file, kind) :: !files
  in
  List.iter (function
      | Variable (_, "added", List (_, list) ) ->
          List.iter (function
              | Option (_, String (_, file),
                        [ String (_, info) ]) ->
                  add_file file info
              | _ -> assert false
            ) list
      | Variable (_, "added",
                  Option (_, String (_, file),
                          [ String (_, info) ])
                 ) ->
          add_file file info

      | Variable (_, name, v ) ->
          Printf.eprintf "unexpected %S of kind %s\n%!"
            name (kind v)
      | _ -> assert false) opamfile.file_contents;
  !files
end

module META = struct

  open Meta_file.Types

  let ocaml_directory = "lib/ocaml"

  let get_directory meta_file meta_dir =
    match StringMap.find "directory" meta_file.p_variables with
    | exception Not_found ->  meta_dir
    | { var_assigns = [ [], directory ] ; _ } ->
        let meta_dir =
            match EzString.chop_prefix directory ~prefix:"^" with
            | Some directory -> ocaml_directory // directory
            | None ->
                match EzString.chop_prefix directory ~prefix:"+" with
                | Some directory -> ocaml_directory // directory
                | None ->
                    meta_dir // directory
        in
        meta_dir
    | _ -> assert false

  let rec create ?meta_parent state ~meta_name ~meta_file ~meta_opam ~meta_dir =
    if StringMap.mem meta_name state.meta_packages then
      Printf.kprintf failwith "Duplicated meta package %s" meta_name;
    let meta_dir = get_directory meta_file meta_dir in
    let m = {
      meta_name ;
      meta_file ;
      meta_opam ;
      meta_dir ;
      meta_parent ;
      meta_subs = [] ;
      meta_deps = [] ;
      meta_revdeps = [] ;
      meta_libs = [] ;
    } in
    state.meta_packages <- StringMap.add meta_name m state.meta_packages;
    meta_opam.opam_metas <- m :: meta_opam.opam_metas;

    let meta_parent = m in
    List.iter (fun (name, meta_file) ->
        let meta_name = meta_name ^ "." ^ name in
        let m = create state ~meta_parent ~meta_name ~meta_file
            ~meta_opam ~meta_dir in
        meta_parent.meta_subs <- m :: meta_parent.meta_subs
      ) meta_file.p_packages;
    m

  let trim_requires s =
    let len = String.length s in
    let b = Bytes.create len in
    for i = 0 to len-1 do
      let c = match s.[i] with
        | '\n' -> ' '
        | ',' -> ' '
        | c -> c
      in
      Bytes.set b i c
    done;
    Bytes.to_string b



  let find_requires state m =
    match StringMap.find "requires" m.meta_file.p_variables with
    | exception Not_found -> ()
    | v ->
        List.iter (fun (_, requires) ->
            List.iter (fun name ->
                match StringMap.find name state.meta_packages with
                | exception Not_found ->
                    Printf.eprintf "Unknown meta %S required by %S\n%!"
                      name m.meta_name
                | dep ->
                    m.meta_deps <- dep :: m.meta_deps ;
                    dep.meta_revdeps <- m :: dep.meta_revdeps
              ) ( EzString.split_simplify (trim_requires requires) ' ' )
          ) ( v.var_assigns @ v.var_additions )

end

module LIB = struct

  let create ?lib_dir state lib_opam ~lib_name ~lib_kind =
    let long_name = lib_opam.opam_name ^ "::" ^ lib_name in
    match Hashtbl.find state.ocaml_libraries long_name with
    | exception Not_found ->
        let lib = {
          lib_name ;
          lib_dir ;
          lib_modules = [];
          lib_interfaces = [];
          lib_kinds = [ lib_kind ] ;
          lib_opam ;
        } in
        Hashtbl.add state.ocaml_libraries long_name lib;
        lib_opam.opam_libs <-
          StringMap.add lib_name lib lib_opam.opam_libs ;
        lib
    | lib ->
        if lib.lib_opam != lib_opam then
          Printf.kprintf failwith "Library %s defined by opam %S and %S"
            lib_name lib.lib_opam.opam_name lib_opam.opam_name;
        lib.lib_kinds <- lib_kind :: lib.lib_kinds;
        lib

end

module MOD = struct

  let create ~mod_name ~mod_file ~mod_kind state ~mod_opam =
    let long_name = mod_opam.opam_name ^ "::" ^ mod_name in
    match Hashtbl.find state.ocaml_modules long_name with
    | exception Not_found ->

        let m = {
          mod_name ;
          mod_file ;
          mod_opam ;
          mod_kinds = [ mod_kind ];
        } in
        Hashtbl.add state.ocaml_modules long_name m ;
        Hashtbl.add state.ocaml_modules mod_name m ;
        m

    | m ->
        m.mod_kinds <- mod_kind :: m.mod_kinds ;
        m

end

let main () =

  let state = {
    opam_packages = StringMap.empty ;
    meta_packages = StringMap.empty ;
    ocaml_libraries = Hashtbl.create 13 ;
    ocaml_modules = Hashtbl.create 13 ;
  } in

  let opam_switch_prefix = try Sys.getenv "OPAM_SWITCH_PREFIX"
    with Not_found -> failwith "not in an opam switch"
  in
  let changes_dir = opam_switch_prefix // ".opam-switch" // "install" in
  let files = try Sys.readdir changes_dir with _ -> [||] in
  let packages = ref [] in
  Array.iter (fun file ->
      match EzString.chop_suffix file ~suffix:".changes" with
      | None -> ()
      | Some name ->
          let changes = OPAM.parse_changes ( changes_dir // file ) in
          packages := ( name, changes ) :: !packages
    ) files;

  let check_file opam_package file =

    let dirname = Filename.dirname file in
    let basename = Filename.basename file in
    if basename = "META" then
      let filename = opam_switch_prefix // file in
      let meta_name = Meta_file.Parser.name_of_META file in
      Printf.eprintf "opam package %S DEFINES ocamlfind package %S\n%!"
        opam_package.opam_name meta_name ;

      let meta_file = Meta_file.Parser.parse_file filename in
      let _m =
        META.create state ~meta_name ~meta_file
          ~meta_opam:opam_package ~meta_dir:dirname in
      ()
    else
      match EzString.chop_suffix basename ~suffix:".cma" with
      | Some lib_name ->
          let _lib =
            LIB.create state opam_package
              ~lib_name ~lib_dir:dirname ~lib_kind:CMA
          in
          ()
      | None ->
          match EzString.chop_suffix basename ~suffix:".cmxa" with
          | Some lib_name ->
              let _lib =
                LIB.create state opam_package
                  ~lib_name ~lib_dir:dirname ~lib_kind:CMXA
              in
              ()
          | None ->
              match EzString.chop_suffix basename ~suffix:".mli" with
              | Some mod_name ->
                  let mod_name = String.capitalize mod_name in
                  let mod_file = Filename.chop_suffix file ".mli" in
                  let _m =
                    MOD.create state
                      ~mod_name ~mod_file ~mod_kind:MLI
                      ~mod_opam:opam_package
                  in
                  ()
              | None ->
                  ()
  in

  List.iter (fun (opam_name, opam_files) ->
      let opam_package = OPAM.create state opam_name opam_files in
      List.iter (fun (file, _kind) ->
          check_file opam_package file
        ) opam_files
    ) !packages ;

  StringMap.iter (fun _ meta_package ->

      META.find_requires state meta_package

    ) state.meta_packages ;

  for i = 1 to Array.length Sys.argv - 1 do
    let m = Sys.argv.(i) in
    match Hashtbl.find_all state.ocaml_modules m with
    | exception Not_found -> failwith "module not found"
    | [ m ] ->
        if List.mem MLI m.mod_kinds then
          Unix.execvp "less" [| "less";
                                opam_switch_prefix //
                                ( m.mod_file ^ ".mli") |]
        else
        if List.mem ML m.mod_kinds then
          Unix.execvp "less" [| "less";
                                opam_switch_prefix //
                                ( m.mod_file ^ ".ml") |]
    | list ->
        List.iter (fun m ->
            Printf.printf "* %s::%s\n%!"
              m.mod_name m.mod_opam.opam_name
          ) list

  done

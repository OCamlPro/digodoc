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
open Types

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
                  Printf.eprintf "Warning: unknown meta %S required by %S\n%!"
                    name m.meta_name
              | dep ->
                  m.meta_deps <- dep :: m.meta_deps ;
                  dep.meta_revdeps <- m :: dep.meta_revdeps
            ) ( EzString.split_simplify (trim_requires requires) ' ' )
        ) ( v.var_assigns @ v.var_additions )

let rec lookup_library m dep archive =
  match StringMap.find archive dep.meta_opam.opam_libs with
  | lib -> lib
  | exception Not_found ->
      let rec iter deps =
        match deps with
        | [] -> raise Not_found
        | dep :: deps ->
            match lookup_library m dep archive with
            | exception Not_found -> iter deps
            | lib -> lib
      in
      iter dep.meta_deps

let lookup_library state m archive =
  match
    try
      lookup_library m m archive
    with
      Not_found ->
        let lib = Hashtbl.find state.ocaml_libraries  "stdlib" in
        StringMap.find archive lib.lib_opam.opam_libs
  with
  | exception Not_found ->
      Printf.eprintf "Warning: could not find archive %S in deps of meta %S\n%!"
        archive m.meta_name
  | lib ->
      (*      Printf.eprintf "meta %S uses library %s::%s\n%!"
              m.meta_name lib.lib_opam.opam_name lib.lib_name; *)
      if not (List.memq lib m.meta_libs) then begin
        m.meta_libs <- lib :: m.meta_libs;
        lib.lib_metas <- m :: lib.lib_metas
      end

let find_archives state m =
  match StringMap.find "archive" m.meta_file.p_variables with
  | exception Not_found -> ()
  | v ->
      List.iter (fun (_, archives) ->
          List.iter (fun archive ->
              if Filename.check_suffix archive ".cma" then
                lookup_library state m
                  (Filename.chop_suffix archive ".cma")
              else
              if Filename.check_suffix archive ".cmxa" then
                lookup_library state m
                  (Filename.chop_suffix archive ".cmxa")
              else
              if Filename.check_suffix archive ".cmxs" then
                lookup_library state m
                  (Filename.chop_suffix archive ".cmxs")
              else
                Printf.eprintf "Warning: meta %S has unknown archive kind %S\n%!"
                  m.meta_name archive
            ) ( EzString.split_simplify (trim_requires archives) ' ' )
        ) ( v.var_assigns @ v.var_additions )

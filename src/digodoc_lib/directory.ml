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
open Type

open Meta_file.Types

let ocaml_directory = "lib/ocaml"

let get state dir_name =
  try
    StringMap.find dir_name state.directories
  with Not_found ->
    let dir = {
      dir_name = dir_name ;
      dir_meta = None ;
      dir_libs = StringMap.empty ;
      dir_mdls = StringMap.empty ;
    } in
    state.directories <- StringMap.add dir_name dir state.directories;
    dir

let of_meta state meta_file meta_dir =
  match StringMap.find "directory" meta_file.p_variables with
  | exception Not_found ->  meta_dir
  | { var_assigns = [ [], directory ] ; _ } ->
      let dir =
        match EzString.chop_prefix directory ~prefix:"^" with
        | Some directory -> ocaml_directory // directory
        | None ->
            match EzString.chop_prefix directory ~prefix:"+" with
            | Some directory -> ocaml_directory // directory
            | None -> meta_dir.dir_name // directory
      in
      get state dir
  | _ -> assert false

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
open Types

open Meta_file.Types

let rec create ?meta_parent state ~meta_name ~meta_file ~meta_opam
    ~meta_dir =
  if StringMap.mem meta_name state.meta_packages then
    Printf.kprintf failwith "Duplicated meta package %s" meta_name;
  let meta_dir = Directory.of_meta state meta_file meta_dir in
  let meta = {
    meta_name ;
    meta_file ;
    meta_opam ;
    meta_dir ;
    meta_parent ;
    meta_subs = [] ;
    meta_deps = StringMap.empty ;
    meta_revdeps = StringMap.empty ;
    meta_archives = StringMap.empty ;
  } in
  state.meta_packages <- StringMap.add meta_name meta state.meta_packages;
  meta_opam.opam_metas <- meta :: meta_opam.opam_metas;

  let meta_parent = meta in
  List.iter (fun (name, meta_file) ->
      let meta_name = meta_name ^ "." ^ name in
      let meta = create state ~meta_parent ~meta_name ~meta_file
          ~meta_opam ~meta_dir in
      meta_parent.meta_subs <- meta :: meta_parent.meta_subs
    ) meta_file.p_packages;
  meta

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

let find_requires state meta =
  match StringMap.find "requires" meta.meta_file.p_variables with
  | exception Not_found -> ()
  | v ->
      List.iter (fun (_, requires) ->
          List.iter (fun name ->
              match StringMap.find name state.meta_packages with
              | exception Not_found ->
                  Printf.eprintf "Warning: unknown meta %S required by %S\n%!"
                    name meta.meta_name
              | dep ->
                  meta.meta_deps <- StringMap.add name dep meta.meta_deps ;
                  dep.meta_revdeps <-
                    StringMap.add meta.meta_name meta dep.meta_revdeps
            ) ( EzString.split_simplify (trim_requires requires) ' ' )
        ) ( v.var_assigns @ v.var_additions )



let rec lookup_archive meta ~dep ~is_library ~basename =
  if basename = "ocamltoplevel" then
    Printf.eprintf "lookup_archive meta:%s dep:%s opam:%s\n"
      meta.meta_name dep.meta_name dep.meta_opam.opam_name;
  if is_library then
    match StringMap.find basename dep.meta_opam.opam_libs with
    | lib -> Archive_lib lib
    | exception Not_found ->
        let rec iter deps =
          match deps with
          | [] -> raise Not_found
          | (_, dep) :: deps ->
              match lookup_archive meta ~dep ~is_library ~basename with
              | exception Not_found -> iter deps
              | lib -> lib
        in
        iter ( StringMap.bindings dep.meta_deps )
  else
    (* TODO for modules, beware of checking ext with .cmx *)
    raise Not_found

let lookup_archive state ~is_library meta ~archive ~basename =
  let arch =
    match
      try
        lookup_archive meta ~dep:meta ~is_library ~basename
      with
        Not_found ->
          if is_library then
            let lib = Hashtbl.find state.ocaml_libs "stdlib" in
            Archive_lib ( StringMap.find basename lib.lib_opam.opam_libs )
          else
            raise Not_found
    with
    | exception Not_found ->
        Printf.eprintf
          "Warning: could not find archive %S (%s) in deps of meta %S\n%!"
          basename (if is_library then "library" else "module") meta.meta_name;
        Archive_missing
    | arch -> arch
  in

  meta.meta_archives <- StringMap.add archive arch meta.meta_archives;
  match arch with
  | Archive_lib lib ->
      lib.lib_metas <- StringMap.add meta.meta_name meta lib.lib_metas
  | Archive_mdl mdl ->
      mdl.mdl_metas <- StringMap.add meta.meta_name meta mdl.mdl_metas
  | Archive_missing -> ()

let find_archives state meta =
  match StringMap.find "archive" meta.meta_file.p_variables with
  | exception Not_found -> ()
  | v ->
      List.iter (fun (_, archives) ->
          List.iter (fun archive ->
              let basename, ext = EzString.rcut_at archive '.' in
              let ext = String.lowercase ext in
              let need_lookup =
                match ext with
                | "cmxa" -> Some true
                | "cmx" -> Some false

                (* discard *)
                | "cmxs"
                | "cmo"
                | "cma" -> None
                | _ ->
                    Printf.eprintf
                      "Warning: meta %S has unknown archive kind %S\n%!"
                      meta.meta_name archive;
                    None
              in
              match need_lookup with
              | None -> ()
              | Some is_library ->
                  lookup_archive state meta ~is_library ~archive ~basename
            ) ( EzString.split_simplify (trim_requires archives) ' ' )
        ) ( v.var_assigns @ v.var_additions )

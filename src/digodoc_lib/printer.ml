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

let print state =
  Printf.printf "OPAM_SWITCH_PREFIX=%s\n" state.opam_switch_prefix;

  Printf.printf "Opam packages:\n";
  StringMap.iter (fun package opam_package ->
      Printf.printf "opam package: %s\n" package;
      Printf.printf "    opam_version: %s\n"
        (match opam_package.opam_version with
         | None -> "" | Some version -> version);
      List.iter (fun m ->
          Printf.printf "    opam_meta: %s\n" m.meta_name
        ) opam_package.opam_metas;
      StringMap.iter (fun _ lib ->
          Printf.printf "    opam_lib: %s\n" lib.lib_name
        ) opam_package.opam_libs;
      StringMap.iter (fun _name dep ->
          Printf.printf "    opam_dep: %s\n%!" dep.opam_name
        ) opam_package.opam_deps;
      StringMap.iter (fun _name dep ->
          Printf.printf "    opam_rev: %s\n%!" dep.opam_name
        ) opam_package.opam_revdeps;
    ) state.opam_packages ;

  Printf.printf "Meta packages:\n";
  StringMap.iter (fun _package m ->
      Printf.printf "Meta package: %s\n" m.meta_name;
      Printf.printf "    meta_opam: %s\n" m.meta_opam.opam_name;
      Printf.printf "    meta_subs: %s\n"
        ( String.concat " "
            (List.map (fun m -> m.meta_name) m.meta_subs)) ;
      Printf.printf "    meta_deps: %s\n"
        ( String.concat " "
            (List.map (fun m -> m.meta_name)
               (StringMap.bindings m.meta_deps |> List.map snd))) ;
      Printf.printf "    meta_revdeps: %s\n"
        ( String.concat " "
            (List.map (fun m -> m.meta_name)
               (StringMap.bindings m.meta_revdeps |> List.map snd))) ;
      Printf.printf "    meta_libs: %s\n"
        ( String.concat " "
            (List.map (fun lib ->
                 lib.lib_opam.opam_name ^ "::" ^ lib.lib_name) m.meta_libs)) ;
    ) state.meta_packages

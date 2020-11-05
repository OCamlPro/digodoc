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

  Printf.printf "\n======================================================\n\n";
  Printf.printf "Opam packages:\n";
  StringMap.iter (fun package opam_package ->
      Printf.printf "opam package: %s\n" package;
      Printf.printf "    opam_version: %s\n"
        (match opam_package.opam_version with
         | None -> "" | Some version -> version);
      Printf.printf "    opam_synopsis: %s\n"
        (match opam_package.opam_synopsis with
         | None -> "" | Some version -> version);
      Printf.printf "    opam_authors: %s\n"
        (match opam_package.opam_authors with
         | None -> "" | Some list ->
             String.concat " " list);
      List.iter (fun m ->
          Printf.printf "    opam_meta: %s\n" m.meta_name
        ) opam_package.opam_metas;

      StringMap.iter (fun _ lib ->
          Printf.printf "    opam_lib: %s\n" lib.lib_name;

          Printf.printf "       lib_dir: %s\n" lib.lib_dir.dir_name ;
          Printf.printf "       lib_units:\n";
          List.iter (fun unit ->
              Printf.printf "          %s\n" unit.unit_name
            ) lib.lib_units;
          Printf.printf "       lib_mdls:\n";
          StringMap.iter (fun _ mdl ->
              Printf.printf "          %s (%s)\n" mdl.mdl_name
                (String.concat ", "(StringSet.to_list mdl.mdl_exts));
            ) lib.lib_mdls;

          if StringMap.is_empty lib.lib_metas then
            Printf.printf "       LIBRARY %s::%s HAS NO META\n"
              opam_package.opam_name lib.lib_name
          else
            StringMap.iter (fun _ meta ->
                Printf.printf "      lib_meta: %s\n" meta.meta_name
              ) lib.lib_metas
        ) opam_package.opam_libs;

      StringMap.iter (fun _ mdl ->
          Printf.printf "    opam_mdl: %s (%s)\n" mdl.mdl_name
            (String.concat ", "(StringSet.to_list mdl.mdl_exts));
          Printf.printf "       module %s is part of %d libs:\n" mdl.mdl_name
            (StringMap.cardinal mdl.mdl_libs);
          StringMap.iter (fun _ lib ->
              Printf.printf "             mdl_lib: %s\n" lib.lib_name
            ) mdl.mdl_libs;
        ) opam_package.opam_mdls;

      StringMap.iter (fun _name dep ->
          Printf.printf "    opam_dep: %s\n%!" dep.opam_name
        ) opam_package.opam_deps;
      StringMap.iter (fun _name dep ->
          Printf.printf "    opam_rev: %s\n%!" dep.opam_name
        ) opam_package.opam_revdeps;
    ) state.opam_packages ;

  Printf.printf "\n======================================================\n\n";
  Printf.printf "Meta packages:\n";
  StringMap.iter (fun _package meta ->
      Printf.printf "Meta package: %s\n" meta.meta_name;
      Printf.printf "    meta_opam: %s\n" meta.meta_opam.opam_name;
      Printf.printf "    meta_subs: %s\n"
        ( String.concat " "
            (List.map (fun meta -> meta.meta_name) meta.meta_subs)) ;
      Printf.printf "    meta_deps: %s\n"
        ( String.concat " "
            (List.map (fun meta -> meta.meta_name)
               (StringMap.bindings meta.meta_deps |> List.map snd))) ;
      Printf.printf "    meta_revdeps: %s\n"
        ( String.concat " "
            (List.map (fun meta -> meta.meta_name)
               (StringMap.bindings meta.meta_revdeps |> List.map snd))) ;
      Printf.printf "    meta_libs: %s\n"
        ( String.concat " "
            (List.map (fun (archive_name, archive) ->
                 match archive with
                 | Archive_lib lib ->
                     lib.lib_opam.opam_name ^ "::" ^ lib.lib_name
                 | Archive_mdl mdl ->
                     mdl.mdl_opam.opam_name ^ "::" ^ mdl.mdl_name
                 | Archive_missing ->
                     Printf.sprintf "MISSING::%s" archive_name
               ) ( StringMap.bindings meta.meta_archives))) ;
    ) state.meta_packages

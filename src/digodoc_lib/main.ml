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

(*
  DONE:
  * find all installed opam packages
  * read changes files to discover ownership of files by opam packages
  * read all META files associated with opam packages
  * associate .cma/.cmxa files to meta packages and opam packages

  TODO:
  * read opam files for direct dependencies between opam packages
  * use ocamlobjinfo to detect modules provided by libraries

*)

open EzCompat
open EzFile.OP
open Types
open Ezcmd.V2

let cache_file = "_digodoc/digodoc.state"

type action =
  | Scan
  | Search of string
  | OpenDoc
  | GenerateHtml
  | GenerateIndex
  | CheckLinks
  | AddTrailer
  | HtmlizeSources of string

let main () =

  let opam_switch_prefix = lazy
    (
      try Sys.getenv "OPAM_SWITCH_PREFIX"
      with Not_found -> failwith "not in an opam switch"
    )
  in

  let get_state ~state ~switch ~objinfo =
    match state with
    | None ->
        let opam_switch_prefix =
          match switch with
          | None -> Lazy.force opam_switch_prefix
          | Some s -> s
        in
        let state = Compute.compute ~opam_switch_prefix ~objinfo () in
        if objinfo then begin
          EzFile.make_dir ~p:true "_digodoc";
          let oc = open_out_bin cache_file in
          output_value oc ( state : state );
          close_out oc;
        end;
        state
    | Some state -> state
  in
  let action = ref None in
  let set_action a =
        match !action with
        | None -> action := Some a
        | Some _ -> failwith "Duplicate actions"
  in
  let arg_set_action a =
    EZCMD.TYPES.Arg.Unit (fun () -> set_action a)
  in
  let objinfo = ref true in
  let cached = ref false in
  let switch = ref None in
  let continue_on_error = ref false in
  EZCMD.parse EZCMD.TYPES.[

      "--no-objinfo", Arg.Clear objinfo,
      " do not call ocamlobjinfo to attach modules to libraries";

      "--no-sources", Arg.Clear Htmlize.Globals.sources,
      "do not generate sources for opam packages";

      "--cached", Arg.Set cached,
      " read cached state";
      "--html", arg_set_action GenerateHtml,
      "build html documentation";

      "--check-links", arg_set_action CheckLinks,
      "check html pages for broken links";

      "--add-trailer", arg_set_action AddTrailer,
      "insert digodoc/ocamlpro trailer";


      "--gen-index", arg_set_action GenerateIndex,
      "generate the index";

      "--www", arg_set_action OpenDoc,
      "open html documentation in a browser" ;

      "-k", Arg.Set continue_on_error,
      "continue on error (mainly odoc)";

      "--continue-on-error", Arg.Set continue_on_error,
      "continue on error (mainly odoc)";

      "--switch-prefix", Arg.String (fun s -> switch := Some s),
      "use SWITCH instead of the current opam switch (ignored if with --cached)";
      "--sources", Arg.String (fun s -> set_action (HtmlizeSources s)),
      "DIR Htmlize sources in DIR";
    ]
    (fun arg -> set_action (Search arg))
    "digodoc [OPTIONS] MODULE";

  let action = match !action with
    | None -> Scan
    | Some a -> a
  in
  let objinfo = !objinfo in
  let cached = !cached in
  let switch = !switch in
  let continue_on_error = !continue_on_error in
  let state =
    if cached then
      let ic = open_in_bin cache_file  in
      let ( state : state ) = input_value ic in
      close_in ic;
      Some state
    else None
  in

  begin match action with
    | Scan ->
        let state = get_state ~state ~objinfo ~switch in
        Printer.print state
    | GenerateHtml ->
        let state = get_state ~state ~objinfo ~switch in
        Odoc.generate ~state ~continue_on_error;
        Index.generate ();
        Html.add_header_footer ()
        (* Html.iter_html ~add_trailer:true Html.digodoc_html_dir *)
    | CheckLinks ->
        Html.iter_html ~check_links:true Html.digodoc_html_dir
    | AddTrailer ->
        Html.iter_html ~add_trailer:true Html.digodoc_html_dir
    | GenerateIndex ->
        Index.generate ()
    | OpenDoc ->
        let index = Html.digodoc_html_dir // "index.html" in
        if Sys.file_exists index then
          Process.call [| "xdg-open" ; index |]
        else begin
          Printf.eprintf
            "Error: Use `digodoc --html` to generate the documentation first.\n";
          exit 2
        end
    | Search mdl ->
        begin
          let state = get_state ~state ~objinfo:false ~switch in
          match Hashtbl.find_all state.ocaml_mdls_by_name mdl with
          | exception Not_found -> failwith "module not found"
          | [ m ] ->
              let opam_switch_prefix = Lazy.force opam_switch_prefix in
              if StringSet.mem "mli" m.mdl_exts then
                Unix.execvp "less" [| "less";
                                      opam_switch_prefix //
                                      ( Module.file m "mli") |]
              else
              if StringSet.mem "ml" m.mdl_exts then
                Unix.execvp "less" [| "less";
                                      opam_switch_prefix //
                                      ( Module.file m "ml") |]
          | list ->
              Printf.printf "Found %d occurences of %S:\n%!"
                ( List.length list) mdl;
              List.iter (fun m ->
                  Printf.printf "* %s::%s\n%!"
                    m.mdl_opam.opam_name m.mdl_name
                ) list;
              exit 0
        end
    | HtmlizeSources dir ->
        Htmlize.Main.htmlize Globals.htmlize_sources_dir [dir]
  end;

  List.iteri  (fun i args ->
      Printf.eprintf "%d failure: %s\n%!" (i+1)
        (String.concat " " ( Array.to_list args))
    ) ( !Process.failures )

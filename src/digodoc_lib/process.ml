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

exception Error of string

let error fmt = Printf.kprintf (fun s -> raise (Error s)) fmt

let call ?(continue_on_error=false) ?(stdout = Unix.stdout) args =
  Printf.eprintf "Calling %s\n%!" (String.concat " " (Array.to_list args));
  let pid = Unix.create_process args.(0) args Unix.stdin stdout Unix.stderr in
  let rec iter () =
    match Unix.waitpid [] pid with
    | exception Unix.Unix_error (EINTR, _, _) -> iter ()
    | _pid, status -> (
        match status with
        | WEXITED 0 -> ()
        | _ ->
            let s =
              Printf.sprintf "Command '%s' exited with error code %s"
                (String.concat " " (Array.to_list args))
                ( match status with
                  | WEXITED n -> string_of_int n
                  | WSIGNALED n -> Printf.sprintf "SIGNAL %d" n
                  | WSTOPPED n -> Printf.sprintf "STOPPED %d" n )
            in
            if continue_on_error then () else error "%s" s
      )
  in
  iter ()

let call_stdout args =
  let stdout =  "/tmp/objinfo.stdout" in
  let oc = open_out stdout in
  call ~stdout:(Unix.descr_of_out_channel oc) args ;
  close_out oc;
  let lines = EzFile.read_lines stdout in
  Sys.remove stdout;
  lines

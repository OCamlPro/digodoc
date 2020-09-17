(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2011-2020 OCamlPro SAS                                  *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(**************************************************************************)

open EzCompat

(* Raw-version of META files: *)

type precondition = string * bool
type preconditions = precondition list

type variable = {
  var_name : string;
  mutable var_assigns : (preconditions * string) list;
  mutable var_additions : (preconditions * string) list;
}

type t = {
  p_parent : t option;
  mutable p_packages : (string * t) list;
  mutable p_variables : variable StringMap.t;
}

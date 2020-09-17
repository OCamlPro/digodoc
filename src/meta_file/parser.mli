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

val create : Types.t option -> Types.t
val get_variable : Types.t -> string -> Types.variable
val parse_file : string -> Types.t


(* Given the filename of the META file, returns the name of the package *)
val name_of_META : string -> string

(**************************************************************************)
(*                                                                        *)
(*    Copyright 2020 OCamlPro & Origin Labs                               *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module HTML : sig


  (* encode a string to Html *)
  val encode_to_buffer : Buffer.t -> string -> unit
  val encode : string -> string

  (* Html parsing and symbolic handling *)
  module TYPES : sig

    (* [Element (tag-name, attributes, children)] or [PCData text] *)
    type xml =
      | Element of (string * (string * string) list * xml list)
      | PCData of string

  end

  module CONS : sig
    type t = ?a:(string * string) list -> TYPES.xml list -> TYPES.xml

    val element : string -> t
    val s : string -> TYPES.xml

    val div : t
    val a : t
    val p : t

    val hr : TYPES.xml

  end

  val check : ?msg:string -> string -> string

  val parse_file : string -> TYPES.xml
  val parse_string : string -> TYPES.xml
  val to_string : TYPES.xml -> string

  type error
  exception Error of error
  val string_of_error : error -> string
end

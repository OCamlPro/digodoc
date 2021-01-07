
type context

type state =
  | Newline
  | String
  | Quotation_p4
  | Quotation_ppx of string
  | Comment
  | Code      (* Code within comment (a.k.a. '{| |}') *)
  | Verbatim  (* Verbatim block within comment (a.k.a. '{v v}') *)

type token_info =
  (Lexing.position * Lexing.position) * string * state list

val initial_state : context

val token : context -> Lexing.lexbuf -> context * Approx_tokens.token

val print_context : Format.formatter -> context -> unit

val tokens_of_string :
  ?st:context -> string -> (Approx_tokens.token * token_info) list
val tokens_of_file :
  ?st:context -> string -> (Approx_tokens.token * token_info) list

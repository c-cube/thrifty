module P = CCParse

type 'a t = 'a P.t

val const_value : Ast.Const_value.t t

(** {2 Utils} *)

val parse_string : 'a t -> string -> ('a, string) result

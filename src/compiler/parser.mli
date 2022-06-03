module P = CCParse

type 'a t = 'a P.t

val const_value : Ast.Const_value.t t
val field_type : Ast.Field_type.t t
val statement : Ast.Statement.t t
val file : Ast.Statement.t list t

(** {2 Utils} *)

val parse_string : 'a t -> string -> ('a, string) result

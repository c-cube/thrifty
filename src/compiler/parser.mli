module P = CCParse

type 'a t = 'a P.t

val parse_string : 'a t -> string -> ('a, string) result

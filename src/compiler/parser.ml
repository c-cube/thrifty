module P = CCParse

type 'a t = 'a P.t

let parse_string = P.parse_string

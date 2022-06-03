module Fmt = CCFormat

let s1 = {|
{ "a": 1, "b": 2}

|}

let c1 = Parser.parse_string Parser.const_value s1;;

Fmt.printf "parsed %a@." (Fmt.Dump.result Ast.Const_value.pp) c1

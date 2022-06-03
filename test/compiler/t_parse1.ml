module Fmt = CCFormat

module T1 = struct
  let s = {|
  // hello
-24
|}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module T2 = struct
  let s = {|
  // hello
  "world!"
|}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module T3 = struct
  let s = {|
{ "x": 1, "is_nice?": true,
      "b": /* 1+1= */ 2}

|}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module T4 = struct
  let s =
    {|
  [ true ; // oh yeah
   42,  -3,
   "12 = 10+2 " /* wut
    ?? */
] |}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

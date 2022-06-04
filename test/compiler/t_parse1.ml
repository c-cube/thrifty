module Fmt = CCFormat

module P_const1 = struct
  let s = {|
  // hello
-24
|}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module P_const2 = struct
  let s = {|
  // hello
  "world!"
|}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module P_const3 = struct
  let s = {|
{ "x": 1, "is_nice?": true,
      "b": /* 1+1= */ 2}

|}

  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module P_const4 = struct
  let s = {| true |}
  let c = Parser.parse_string Parser.const_value s;;

  Fmt.printf "c = %a@." (Fmt.Dump.result Ast.Const_value.pp) c
end

module P_const5 = struct
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

module P_field_ty1 = struct
  let s = {|
    list< map< bool, /* int!! */ i32> >
] |}

  let ty = Parser.parse_string Parser.field_type s;;

  Fmt.printf "ty = %a@." (Fmt.Dump.result Ast.Field_type.pp) ty
end

module P_file1 = struct
  let s =
    {|
  //include "foo";

  //include "bar";
  const i32 x = 42,
  const bool y = false;
    const string my_lovely_str = "hello world !";

  typedef set<bool> truthvals ;

  enum foo { A = 1, B, C = 10; }
  enum foo2 {
    DEAD = 0,
    ALIVE  = 1,
    S_CAT = 10000
  }

  struct bar {
    1: i32 x = 42;
    2: set<bool> all_done ( ohyeah= 'def' ),
    bool yup;
  }

  service Foo {
    void startLaunch(1: i32 missile = 42);

    bool isLaunched();

    oneway void ping() throws (2: set<bool> all_done);

  }

  struct Yolo {
    1: set < i32 (x='x') > swag (swag = 'super');
  } (ocaml = 'yep yep');
 |}

  let f = Parser.parse_string Parser.file s;;

  Fmt.printf "ty = %a@." (Fmt.Dump.result Ast.File.pp) f
end

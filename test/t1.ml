module Fmt = CCFormat

let get_toks, prot = Debug_protocol.debug_write ()

let () =
  let (module P) = prot in
  P.write_struct_begin "Foo";
  P.write_field_begin "x" T_I32 1;
  P.write_i32 42l;
  P.write_field_end ();
  P.write_field_begin "y" T_BOOL 2;
  P.write_bool true;
  P.write_field_end ();
  P.write_struct_end ();
  ()

let () =
  Fmt.printf "tokens: %a@."
    (Fmt.Dump.list Debug_protocol.Token.pp)
    (get_toks ())

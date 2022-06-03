module Fmt = CCFormat
open Types

let prot = Debug_protocols.debug_write ()

let () =
  prot#write_struct_begin "Foo";
  prot#write_field_begin "x" T_I32 1;
  prot#write_i32 42l;
  prot#write_field_end;
  prot#write_field_begin "y" T_BOOL 2;
  prot#write_bool true;
  prot#write_field_end;
  prot#write_struct_end;
  ()

let () =
  Fmt.printf "tokens: %a@."
    (Fmt.Dump.list Debug_protocols.Token.pp)
    prot#get_tokens

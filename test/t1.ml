module Fmt = CCFormat

let prot = Debug.write
let toks = ref []

let () =
  prot.write_struct_begin toks "Foo";
  prot.write_field_begin toks "x" T_I32 1;
  prot.write_i32 toks 42l;
  prot.write_field_end toks;
  prot.write_field_begin toks "y" T_BOOL 2;
  prot.write_bool toks true;
  prot.write_field_end toks;
  prot.write_struct_end toks;
  ()

let toks = List.rev !toks
let () = Fmt.printf "tokens: %a@." (Fmt.Dump.list Debug.Token.pp) toks

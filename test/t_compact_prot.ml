module Fmt = CCFormat
module Prot = Compact_protocol

let () = Printexc.record_backtrace true
let buf = Buffer.create 32
let tr_buf = Basic_transports.transport_of_buffer buf
let proto_write = Prot.write (tr_buf :> transport_write)

let () =
  let (module P) = proto_write in
  P.write_msg_begin "hello" MSG_ONEWAY 42;
  P.write_msg_end ();
  P.write_struct_begin "S0";
  P.write_field_begin "x" T_I32 1;
  P.write_i32 17l;
  P.write_field_end ();
  P.write_field_begin "y" T_STRING 2;
  P.write_string "hello";
  P.write_field_end ();
  P.write_field_stop ();
  P.write_struct_end ();
  ()

let data = Buffer.contents buf
let () = Fmt.printf "encoded data (%d bytes): %S@." (String.length data) data
let proto_read = Prot.read (Basic_transports.transport_of_string data)

let () =
  let (module P) = proto_read in
  (let name, ty, seq = P.read_msg_begin () in
   Fmt.printf "msg name=%s seq=%d@." name seq;
   assert (name = "hello");
   assert (ty = MSG_ONEWAY);
   assert (seq = 42));
  let _name, ty, id = P.read_field_begin () in
  Printf.printf "read field %S ty=%s id=%d\n%!" _name
    (string_of_element_type ty)
    id;
  assert (ty = T_I32);
  assert (id = 1);
  let i = P.read_i32 () in
  P.read_field_end ();
  Fmt.printf "got field x@.";
  assert (i = 17l);
  let _name, ty, id = P.read_field_begin () in
  assert (ty = T_STRING || ty = T_BINARY);
  assert (id = 2);
  let s = P.read_string () in
  P.read_field_end ();
  assert (s = "hello");
  Fmt.printf "got field y@.";
  (let finish =
     try
       ignore (P.read_field_begin () : _ * _ * _);
       false
     with Read_stop_field -> true
   in
   assert finish);
  P.read_msg_end ();
  Fmt.printf "got end of message@.";
  ()

(* now use the debug protocol to read again *)

let proto_read = Prot.read (Basic_transports.transport_of_string data)
let get_toks, prot_debug = Debug_protocol.debug_write ()

let toks =
  Format.printf "transfer to debug protocol@.";
  Transfer.transfer_message proto_read prot_debug;
  Transfer.transfer_struct proto_read prot_debug;
  get_toks ()

let () =
  Fmt.printf "tokens: %a@."
    (Fmt.Dump.list Debug_protocol.Token.pp)
    (get_toks ())

let dbg_read = Debug_protocol.debug_read toks

let data' =
  (* transfer from tokens back into compact bytes *)
  let buf = Buffer.create 32 in
  let proto_write = Prot.write (Basic_transports.transport_of_buffer buf) in
  Transfer.transfer_message dbg_read proto_write;
  Transfer.transfer_struct dbg_read proto_write;
  Buffer.contents buf

let () = Printf.printf "re-encoded tokens: %S\n" data'
let () = Printf.printf "data = data'? %b\n" (data = data')

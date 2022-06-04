module Fmt = CCFormat

let () = Printexc.record_backtrace true
let tr_buf = new Basic_transports.transport_buffer
let proto_write = Binary_protocol.write (tr_buf :> transport_write)

let () =
  let p = proto_write in
  p#write_msg_begin "hello" MSG_ONEWAY 42;
  p#write_field_begin "x" T_I32 1;
  p#write_i32 17l;
  p#write_field_end;
  p#write_field_begin "y" T_STRING 2;
  p#write_string "hello";
  p#write_field_end;
  p#write_field_stop;
  p#write_msg_end

let data = tr_buf#contents
let () = Fmt.printf "encoded data (%d bytes): %S@." (String.length data) data

let proto_read =
  Binary_protocol.read (Basic_transports.transport_of_string data)

let () =
  let p = proto_read in
  (let name, ty, seq = p#read_msg_begin in
   Fmt.printf "msg name=%s seq=%d@." name seq;
   assert (name = "hello");
   assert (ty = MSG_ONEWAY);
   assert (seq = 42));
  let _name, ty, id = p#read_field_begin in
  assert (ty = T_I32);
  assert (id = 1);
  let i = p#read_i32 in
  p#read_field_end;
  Fmt.printf "got field x@.";
  assert (i = 17l);
  let _name, ty, id = p#read_field_begin in
  assert (ty = T_STRING || ty = T_BINARY);
  assert (id = 2);
  let s = p#read_string in
  p#read_field_end;
  assert (s = "hello");
  Fmt.printf "got field y@.";
  (let finish =
     try
       ignore p#read_field_begin;
       false
     with Read_stop_field -> true
   in
   assert finish);
  p#read_msg_end;
  Fmt.printf "got end of message@.";
  ()

module Fmt = CCFormat
module Prot = CompactProtocol

let () = Printexc.record_backtrace true
let buf = Buffer.create 32

let () =
  let proto_wr = Prot.write TBuffer.write in
  proto_wr.write_msg_begin buf "hello" MSG_ONEWAY 42;
  proto_wr.write_msg_end buf;
  proto_wr.write_struct_begin buf "S0";
  proto_wr.write_field_begin buf "x" T_I32 1;
  proto_wr.write_i32 buf 17l;
  proto_wr.write_field_end buf;
  proto_wr.write_field_begin buf "y" T_STRING 2;
  proto_wr.write_string buf "hello";
  proto_wr.write_field_end buf;
  proto_wr.write_field_stop buf;
  proto_wr.write_struct_end buf;
  ()

let data = Buffer.contents buf
let () = Fmt.printf "encoded data (%d bytes): %S@." (String.length data) data

let () =
  let rd = TString.create_reader data in
  let proto_rd = Prot.read TString.read in
  (let name, ty, seq = proto_rd.read_msg_begin rd in
   Fmt.printf "msg name=%s seq=%d@." name seq;
   assert (name = "hello");
   assert (ty = MSG_ONEWAY);
   assert (seq = 42));
  let _name, ty, id = proto_rd.read_field_begin rd in
  Printf.printf "read field %S ty=%s id=%d\n%!" _name
    (string_of_element_type ty)
    id;
  assert (ty = T_I32);
  assert (id = 1);
  let i = proto_rd.read_i32 rd in
  proto_rd.read_field_end rd;
  Fmt.printf "got field x@.";
  assert (i = 17l);
  let _name, ty, id = proto_rd.read_field_begin rd in
  assert (ty = T_STRING || ty = T_BINARY);
  assert (id = 2);
  let s = proto_rd.read_string rd in
  proto_rd.read_field_end rd;
  assert (s = "hello");
  Fmt.printf "got field y@.";
  (let finish =
     try
       ignore (proto_rd.read_field_begin rd : _ * _ * _);
       false
     with Read_stop_field -> true
   in
   assert finish);
  proto_rd.read_msg_end rd;
  Fmt.printf "got end of message@.";
  ()

(* now use the debug protocol to read again *)

let toks =
  Format.printf "transfer to debug protocol@.";
  let toks = ref [] in
  let rd = TString.create_reader data in
  let proto_rd = Prot.read TString.read in
  Transfer.transfer_message proto_rd Debug.write rd toks;
  Transfer.transfer_struct proto_rd Debug.write rd toks;
  List.rev !toks

let () = Fmt.printf "tokens: %a@." (Fmt.Dump.list Debug.Token.pp) toks

let data' =
  (* transfer from tokens back into compact bytes *)
  let buf = Buffer.create 32 in
  let proto_wr = Prot.write TBuffer.write in
  let rd = ref toks in
  Transfer.transfer_message Debug.read proto_wr rd buf;
  Transfer.transfer_struct Debug.read proto_wr rd buf;
  Buffer.contents buf

let () = Printf.printf "re-encoded tokens: %S\n" data'
let () = Printf.printf "data = data'? %b\n" (data = data')

let toks' =
  let toks = ref [] in
  let rd = TString.create_reader data in
  let proto_rd = Prot.read TString.read in
  Transfer.transfer_message proto_rd Debug.write rd toks;
  Transfer.transfer_struct proto_rd Debug.write rd toks;
  List.rev !toks

let () = Fmt.printf "tokens': %a@." (Fmt.Dump.list Debug.Token.pp) toks'

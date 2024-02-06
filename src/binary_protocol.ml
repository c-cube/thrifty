open Types
open Common_

let int_of_field_ty = function
  | T_BOOL -> 2
  | T_BYTE | T_I8 -> 3
  | T_DOUBLE -> 4
  | T_I16 -> 6
  | T_I32 -> 8
  | T_I64 -> 10
  | T_STRING | T_BINARY -> 11
  | T_STRUCT -> 12
  | T_MAP -> 13
  | T_SET -> 14
  | T_LIST -> 15

let field_ty_of_int = function
  | 2 -> T_BOOL
  | 3 -> T_BYTE
  | 4 -> T_DOUBLE
  | 6 -> T_I16
  | 8 -> T_I32
  | 10 -> T_I64
  | 11 -> T_BINARY
  | 12 -> T_STRUCT
  | 13 -> T_MAP
  | 14 -> T_SET
  | 15 -> T_LIST
  | _i -> failwith (Printf.sprintf "invalid type identifier: %d" _i)

let int_of_element_type = int_of_field_ty
let element_type_of_int = field_ty_of_int

let int_of_message_type = function
  | MSG_CALL -> 1
  | MSG_REPLY -> 2
  | MSG_EXCEPTION -> 3
  | MSG_ONEWAY -> 4

let message_type_of_int = function
  | 1 -> MSG_CALL
  | 2 -> MSG_REPLY
  | 3 -> MSG_EXCEPTION
  | 4 -> MSG_ONEWAY
  | _ -> failwith "invalid message type"

let write (type wr) (tr : wr transport_write) : wr protocol_write =
  let b = Bytes.create 8 in
  let write_i16 wr i =
    Bytes.set_int16_be b 0 i;
    tr.write wr b 0 2
  in
  let write_i32 wr i =
    Bytes.set_int32_be b 0 i;
    tr.write wr b 0 4
  in
  let write_i64 wr i =
    Bytes.set_int64_be b 0 i;
    tr.write wr b 0 8
  in
  let write_byte wr c = tr.write_byte wr c in
  let write_binary wr s =
    let n = String.length s in
    write_i32 wr (Int32.of_int n);
    tr.write wr (Bytes.unsafe_of_string s) 0 n
  in
  let write_string wr s = write_binary wr s in
  let write_msg_begin wr name ty seq =
    (* use the strict encoding *)
    let v = 0b1000_0000_0000_0001 in
    (* write as unsigned *)
    tr.write_byte wr (Char.unsafe_chr (v lsr 8));
    tr.write_byte wr (Char.unsafe_chr (v land 0xff));
    (* unused byte *)
    tr.write_byte wr (Char.unsafe_chr 0);
    let msg_type = int_of_message_type ty in
    tr.write_byte wr (Char.unsafe_chr msg_type);
    write_string wr name;
    write_i32 wr (Int32.of_int seq)
  in
  let write_msg_end _wr = () in
  let write_struct_begin _wr _s = () in
  let write_struct_end wr = tr.flush wr in
  let flush wr = tr.flush wr in
  let write_field_begin wr _name ty id =
    (* fits in 16 bits *)
    tr.write_byte wr (Char.unsafe_chr @@ int_of_field_ty ty);
    write_i16 wr id
  in
  let write_field_end _wr = () in
  let write_field_stop wr = tr.write_byte wr (Char.unsafe_chr 0) in
  let write_map_begin wr tyk tyv sz =
    tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type tyk);
    tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type tyv);
    write_i32 wr (Int32.of_int sz)
  in
  let write_map_end _wr = () in
  let write_list_begin wr ty sz =
    tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type ty);
    write_i32 wr (Int32.of_int sz)
  in
  let write_list_end _wr = () in
  let write_set_begin wr ty sz =
    tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type ty);
    write_i32 wr (Int32.of_int sz)
  in
  let write_set_end _wr = () in
  let write_bool wr b = tr.write_byte wr (Char.unsafe_chr (int_of_bool b)) in
  let write_double wr f =
    let i = Int64.bits_of_float f in
    write_i64 wr i
  in
  {
    write_msg_begin;
    write_msg_end;
    write_struct_begin;
    write_struct_end;
    write_field_begin;
    write_field_end;
    write_field_stop;
    write_map_begin;
    write_map_end;
    write_list_begin;
    write_list_end;
    write_set_begin;
    write_set_end;
    write_bool;
    write_byte;
    write_i16;
    write_i32;
    write_i64;
    write_double;
    write_string;
    write_binary;
    flush;
  }

let read (type rd) (tr : rd transport_read) : rd protocol_read =
  let b = Bytes.create 8 in
  let read_bool rd : bool = Char.code (tr.read_byte rd) <> 0 in
  let read_byte rd : char = tr.read_byte rd in
  let read_i16 rd : int =
    really_read tr rd b 0 2;
    Bytes.get_int16_be b 0
  in
  let read_i32 rd : int32 =
    really_read tr rd b 0 4;
    Bytes.get_int32_be b 0
  in
  let read_i64 rd : int64 =
    really_read tr rd b 0 8;
    Bytes.get_int64_be b 0
  in
  let read_double rd : float =
    let i = read_i64 rd in
    Int64.float_of_bits i
  in
  let read_string rd : string =
    let len = read_i32 rd |> Int32.to_int in
    let str = Bytes.create len in
    really_read tr rd str 0 len;
    Bytes.unsafe_to_string str
  in
  let read_binary = read_string in
  let read_msg_begin rd : string * message_type * sequence_number =
    really_read tr rd b 0 4;

    let first_byte = Char.code @@ Bytes.get b 0 in
    if first_byte land 0b1000_0000 <> 0 then (
      (* strict encoding, first bit is set *)
      let version = (first_byte lsl 8) lor Char.code (Bytes.get b 1) in
      if version <> 0b1000_0000_0000_0001 then
        failwith @@ Printf.sprintf "unknown message version %xd" version;
      let ty = Char.code (Bytes.get b 3) |> message_type_of_int in
      let name = read_string rd in
      let seq = read_i32 rd |> Int32.to_int in
      name, ty, seq
    ) else (
      (* old encoding *)
      let len = Bytes.get_int32_be b 0 |> Int32.to_int in
      let name = Bytes.create len in
      really_read tr rd name 0 len;
      let name = Bytes.unsafe_to_string name in
      let ty = Char.code (tr.read_byte rd) |> message_type_of_int in
      let seq = read_i32 rd |> Int32.to_int in
      name, ty, seq
    )
  in
  let read_msg_end _ = () in
  let read_struct_begin _ = "" in
  let read_struct_end _ = () in
  let read_field_begin rd : string * field_type * field_id =
    let ty = Char.code (tr.read_byte rd) in
    if ty = 0 then raise Read_stop_field;
    let ty = field_ty_of_int ty in
    let id = read_i16 rd in
    "", ty, id
  in
  let read_field_end _ = () in
  let read_map_begin rd : element_type * element_type * size =
    let ty_k = Char.code @@ tr.read_byte rd in
    let ty_k = element_type_of_int ty_k in
    let ty_v = Char.code @@ tr.read_byte rd in
    let ty_v = element_type_of_int ty_v in
    let sz = read_i32 rd in
    ty_k, ty_v, Int32.to_int sz
  in
  let read_map_end _ = () in
  let read_list_begin rd : element_type * size =
    let ty = Char.code @@ tr.read_byte rd in
    let ty = element_type_of_int ty in
    let sz = read_i32 rd in
    ty, Int32.to_int sz
  in
  let read_list_end _ = () in
  let read_set_begin rd : element_type * size =
    let ty = Char.code @@ tr.read_byte rd in
    let ty = element_type_of_int ty in
    let sz = read_i32 rd in
    ty, Int32.to_int sz
  in
  let read_set_end _ = () in
  {
    read_msg_begin;
    read_msg_end;
    read_struct_begin;
    read_struct_end;
    read_field_begin;
    read_field_end;
    read_map_begin;
    read_map_end;
    read_list_begin;
    read_list_end;
    read_set_begin;
    read_set_end;
    read_bool;
    read_byte;
    read_i16;
    read_i32;
    read_i64;
    read_double;
    read_string;
    read_binary;
  }

let protocol : protocol = { read; write }

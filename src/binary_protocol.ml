open Types

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
  | _i -> failwith (Printf.sprintf "invalid type identifier %d" _i)

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

let write (tr : transport_write) : protocol_write =
  let b = Bytes.create 8 in
  let (module Tr) = tr in
  let module M = struct
    let write_i16 i =
      Bytes.set_int16_be b 0 i;
      Tr.write b 0 2

    let write_i32 i =
      Bytes.set_int32_be b 0 i;
      Tr.write b 0 4

    let write_i64 i =
      Bytes.set_int64_be b 0 i;
      Tr.write b 0 8

    let write_byte c = Tr.write_byte c

    let write_binary s =
      let n = String.length s in
      write_i32 (Int32.of_int n);
      Tr.write (Bytes.unsafe_of_string s) 0 n

    let write_string s = write_binary s

    let write_msg_begin name ty seq =
      (* use the strict encoding *)
      let v = 0b1000_0000_0000_0001 in
      (* write as unsigned *)
      Tr.write_byte (Char.unsafe_chr (v lsr 8));
      Tr.write_byte (Char.unsafe_chr (v land 0xff));
      (* unused byte *)
      Tr.write_byte (Char.unsafe_chr 0);
      let msg_type = int_of_message_type ty in
      Tr.write_byte (Char.unsafe_chr msg_type);
      write_string name;
      write_i32 (Int32.of_int seq)

    let write_msg_end () = ()
    let write_struct_begin _s = ()
    let write_struct_end () = ()

    let write_field_begin _name ty id =
      (* fits in 16 bits *)
      Tr.write_byte (Char.unsafe_chr @@ int_of_field_ty ty);
      write_i16 id

    let write_field_end () = ()
    let write_field_stop () = Tr.write_byte (Char.unsafe_chr 0)

    let write_map_begin tyk tyv sz =
      Tr.write_byte (Char.unsafe_chr @@ int_of_element_type tyk);
      Tr.write_byte (Char.unsafe_chr @@ int_of_element_type tyv);
      write_i32 (Int32.of_int sz)

    let write_map_end () = ()

    let write_list_begin ty sz =
      Tr.write_byte (Char.unsafe_chr @@ int_of_element_type ty);
      write_i32 (Int32.of_int sz)

    let write_list_end () = ()

    let write_set_begin ty sz =
      Tr.write_byte (Char.unsafe_chr @@ int_of_element_type ty);
      write_i32 (Int32.of_int sz)

    let write_set_end () = ()

    let write_bool b =
      Tr.write_byte
        (Char.unsafe_chr
           (if b then
             1
           else
             0))

    let write_double f =
      let i = Int64.bits_of_float f in
      write_i64 i
  end in
  (module M)

let read (tr : transport_read) : protocol_read =
  let b = Bytes.create 8 in
  let (module Tr) = tr in
  let module M = struct
    let read_bool () : bool = Char.code (Tr.read_byte ()) <> 0
    let read_byte () : char = Tr.read_byte ()

    let read_i16 () : int =
      really_read tr b 0 2;
      Bytes.get_int16_be b 0

    let read_i32 () : int32 =
      really_read tr b 0 4;
      Bytes.get_int32_be b 0

    let read_i64 () : int64 =
      really_read tr b 0 8;
      Bytes.get_int64_be b 0

    let read_double () : float =
      let i = read_i64 () in
      Int64.float_of_bits i

    let read_string () : string =
      let len = read_i32 () |> Int32.to_int in
      let str = Bytes.create len in
      really_read tr str 0 len;
      Bytes.unsafe_to_string str

    let read_binary = read_string

    let read_msg_begin () : string * message_type * sequence_number =
      really_read tr b 0 4;

      let first_byte = Char.code @@ Bytes.get b 0 in
      if first_byte land 0b1000_0000 <> 0 then (
        (* strict encoding, first bit is set *)
        let version = (first_byte lsl 8) lor Char.code (Bytes.get b 1) in
        if version <> 0b1000_0000_0000_0001 then
          failwith @@ Printf.sprintf "unknown message version %xd" version;
        let ty = Char.code (Bytes.get b 3) |> message_type_of_int in
        let name = read_string () in
        let seq = read_i32 () |> Int32.to_int in
        name, ty, seq
      ) else (
        (* old encoding *)
        let len = Bytes.get_int32_be b 0 |> Int32.to_int in
        let name = Bytes.create len in
        really_read tr name 0 len;
        let name = Bytes.unsafe_to_string name in
        let ty = Char.code (Tr.read_byte ()) |> message_type_of_int in
        let seq = read_i32 () |> Int32.to_int in
        name, ty, seq
      )

    let read_msg_end () = ()
    let read_struct_begin () = ""
    let read_struct_end () = ()

    let read_field_begin () : string * field_type * field_id =
      let ty = Char.code (Tr.read_byte ()) in
      if ty = 0 then raise Read_stop_field;
      let ty = field_ty_of_int ty in
      let id = read_i16 () in
      "", ty, id

    let read_field_end () = ()

    let read_map_begin () : element_type * element_type * size =
      let ty_k = Char.code @@ Tr.read_byte () in
      let ty_k = element_type_of_int ty_k in
      let ty_v = Char.code @@ Tr.read_byte () in
      let ty_v = element_type_of_int ty_v in
      let sz = read_i16 () in
      ty_k, ty_v, sz

    let read_map_end () = ()

    let read_list_begin () : element_type * size =
      let ty = Char.code @@ Tr.read_byte () in
      let ty = element_type_of_int ty in
      let sz = read_i16 () in
      ty, sz

    let read_list_end () = ()

    let read_set_begin () : element_type * size =
      let ty = Char.code @@ Tr.read_byte () in
      let ty = element_type_of_int ty in
      let sz = read_i16 () in
      ty, sz

    let read_set_end () = ()
  end in
  (module M)

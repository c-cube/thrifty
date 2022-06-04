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
  | _ -> failwith "invalid type identifier"

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

class writer (tr : transport_write) =
  object (self)
    inherit protocol_write
    val b = Bytes.create 8

    method write_msg_begin name ty seq =
      (* use the strict encoding *)
      let v = 0b1000_0000_0000_0001 in
      (* write as unsigned *)
      tr#write_byte (Char.unsafe_chr (v lsr 8));
      tr#write_byte (Char.unsafe_chr (v land 0xff));
      (* unused byte *)
      tr#write_byte (Char.unsafe_chr 0);
      let msg_type = int_of_message_type ty in
      tr#write_byte (Char.unsafe_chr msg_type);
      self#write_string name;
      self#write_i32 (Int32.of_int seq)

    method write_msg_end = ()
    method write_struct_begin _s = ()
    method write_struct_end = ()

    method write_field_begin _name ty id =
      (* fits in 16 bits *)
      tr#write_byte (Char.unsafe_chr @@ int_of_field_ty ty);
      self#write_i16 id

    method write_field_end = ()
    method write_field_stop = tr#write_byte (Char.unsafe_chr 0)

    method write_map_begin tyk tyv sz =
      tr#write_byte (Char.unsafe_chr @@ int_of_element_type tyk);
      tr#write_byte (Char.unsafe_chr @@ int_of_element_type tyv);
      self#write_i32 (Int32.of_int sz)

    method write_map_end = ()

    method write_list_begin ty sz =
      tr#write_byte (Char.unsafe_chr @@ int_of_element_type ty);
      self#write_i32 (Int32.of_int sz)

    method write_list_end = ()

    method write_set_begin ty sz =
      tr#write_byte (Char.unsafe_chr @@ int_of_element_type ty);
      self#write_i32 (Int32.of_int sz)

    method write_set_end = ()

    method write_bool b =
      tr#write_byte
        (Char.unsafe_chr
           (if b then
             1
           else
             0))

    method write_byte c = tr#write_byte c

    method write_i16 i =
      Bytes.set_int16_be b 0 i;
      tr#write b 0 2

    method write_i32 i =
      Bytes.set_int32_be b 0 i;
      tr#write b 0 4

    method write_i64 i =
      Bytes.set_int64_be b 0 i;
      tr#write b 0 8

    method write_double f =
      let i = Int64.bits_of_float f in
      self#write_i64 i

    method write_binary s =
      let n = String.length s in
      self#write_i32 (Int32.of_int n);
      tr#write (Bytes.unsafe_of_string s) 0 n

    method write_string s = self#write_binary s
  end

class reader (tr : transport_read) =
  object (self)
    inherit protocol_read
    val b = Bytes.create 8

    method read_msg_begin : string * message_type * sequence_number =
      tr#really_read b 0 4;

      let first_byte = Char.code @@ Bytes.get b 0 in
      if first_byte land 0b1000_0000 <> 0 then (
        (* strict encoding, first bit is set *)
        let version = (first_byte lsl 8) lor Char.code (Bytes.get b 1) in
        if version <> 0b1000_0000_0000_0001 then
          failwith @@ Printf.sprintf "unknown message version %xd" version;
        let ty = Char.code (Bytes.get b 3) |> message_type_of_int in
        let name = self#read_string in
        let seq = self#read_i32 |> Int32.to_int in
        name, ty, seq
      ) else (
        (* old encoding *)
        let len = Bytes.get_int32_be b 0 |> Int32.to_int in
        let name = Bytes.create len in
        tr#really_read name 0 len;
        let name = Bytes.unsafe_to_string name in
        let ty = Char.code tr#read_byte |> message_type_of_int in
        let seq = self#read_i32 |> Int32.to_int in
        name, ty, seq
      )

    method read_msg_end = ()
    method read_struct_begin = ""
    method read_struct_end = ()

    method read_field_begin : string * field_type * field_id =
      let ty = Char.code tr#read_byte in
      if ty = 0 then raise Read_stop_field;
      let ty = field_ty_of_int ty in
      let id = self#read_i16 in
      "", ty, id

    method read_field_end = ()

    method read_map_begin : element_type * element_type * size =
      let ty_k = Char.code tr#read_byte in
      let ty_k = element_type_of_int ty_k in
      let ty_v = Char.code tr#read_byte in
      let ty_v = element_type_of_int ty_v in
      let sz = self#read_i16 in
      ty_k, ty_v, sz

    method read_map_end = ()

    method read_list_begin : element_type * size =
      let ty = Char.code tr#read_byte in
      let ty = element_type_of_int ty in
      let sz = self#read_i16 in
      ty, sz

    method read_list_end = ()

    method read_set_begin : element_type * size =
      let ty = Char.code tr#read_byte in
      let ty = element_type_of_int ty in
      let sz = self#read_i16 in
      ty, sz

    method read_set_end = ()
    method read_bool : bool = Char.code tr#read_byte <> 0
    method read_byte : char = tr#read_byte

    method read_i16 : int =
      tr#really_read b 0 2;
      Bytes.get_int16_be b 0

    method read_i32 : int32 =
      tr#really_read b 0 4;
      Bytes.get_int32_be b 0

    method read_i64 : int64 =
      tr#really_read b 0 8;
      Bytes.get_int64_be b 0

    method read_double : float =
      let i = self#read_i64 in
      Int64.float_of_bits i

    method read_string : string =
      let len = self#read_i32 |> Int32.to_int in
      Printf.printf "read string %d\n" len;
      let str = Bytes.create len in
      tr#really_read str 0 len;
      Bytes.unsafe_to_string str

    method read_binary : string = self#read_string
  end

let write (tr : transport_write) : protocol_write = new writer tr
let read tr = new reader tr

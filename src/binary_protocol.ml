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

let write (tr : 'wr transport_write) : 'wr protocol_write =
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

    let write_msg_end wr  = () in
    let write_struct_begin wr _s = () in
    let write_struct_end wr  = tr.flush () in
    let flush = tr.flush in

    let write_field_begin wr _name ty id =
      (* fits in 16 bits *)
      tr.write_byte wr (Char.unsafe_chr @@ int_of_field_ty ty);
      write_i16 wr id in

    let write_field_end wr  = () in
    let write_field_stop wr  = tr.write_byte wr (Char.unsafe_chr 0) in

    let write_map_begin wr tyk tyv sz =
      tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type tyk);
      tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type tyv);
      write_i32 wr (Int32.of_int sz) in

     let write_map_end (wr ) = () in

    let write_list_begin wr ty sz =
      tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type ty);
      write_i32 wr (Int32.of_int sz) in

    let write_list_end (wr ) = () in

    let write_set_begin wr ty sz =
      tr.write_byte wr (Char.unsafe_chr @@ int_of_element_type ty);
      write_i32 wr (Int32.of_int sz) in

    let write_set_end (wr ) = () in

    let write_bool wr b =
      tr.write_byte wr 
        (Char.unsafe_chr
           (if b then
             1
           else
             0)) in

    let write_double wr f =
      let i = Int64.bits_of_float f in
      write_i64 wr i in
    {
  write_msg_begin;
  write_msg_end;
  write_struct_begin;
  write_struct_end;
  write_field_begin;
  write_field_end;
  write_field_stop;
      (** Indicate that the struct is done, no more fields will be added to it *)
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

let read (tr : 'rd transport_read) : 'rd protocol_read =
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
      let sz = read_i32 () in
      ty_k, ty_v, Int32.to_int sz

    let read_map_end () = ()

    let read_list_begin () : element_type * size =
      let ty = Char.code @@ Tr.read_byte () in
      let ty = element_type_of_int ty in
      let sz = read_i32 () in
      ty, Int32.to_int sz

    let read_list_end () = ()

    let read_set_begin () : element_type * size =
      let ty = Char.code @@ Tr.read_byte () in
      let ty = element_type_of_int ty in
      let sz = read_i32 () in
      ty, Int32.to_int sz

    let read_set_end () = ()
  end in
  (module M)

let protocol : protocol =
  (module struct
    let read = read
    let write = write
  end)

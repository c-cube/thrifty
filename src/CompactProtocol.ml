open Types
open Common_

module Dec_ = struct
  let varint (tr : _ transport_read) rd : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in
    while !continue do
      let b = Char.code (tr.read_byte rd) in
      let cur = b land 0x7f in
      if cur <> b then (
        (* at least one byte follows this one *)
        (res := Int64.(logor !res (shift_left (of_int cur) !shift)));
        shift := !shift + 7
      ) else if !shift < 63 || b land 0x7f <= 1 then (
        (res := Int64.(logor !res (shift_left (of_int b) !shift)));
        continue := false
      ) else
        failwith "varint: length exceeded"
    done;
    !res

  let zigzag tr rd : int64 =
    let v = varint tr rd in
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))
end

module Enc_ = struct
  let varint (tw : _ transport_write) wr (i : int64) : unit =
    let i = ref i in
    let continue = ref true in
    while !continue do
      let cur = Int64.(logand !i 0x7fL) in
      if cur = !i then (
        continue := false;
        tw.write_byte wr (Char.unsafe_chr Int64.(to_int cur))
      ) else (
        tw.write_byte wr (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
        i := Int64.shift_right_logical !i 7
      )
    done

  let[@inline] int_as_varint tr wr i : unit = varint tr wr (Int64.of_int i)

  let[@inline] zigzag tr wr (i : int64) : unit =
    varint tr wr Int64.(logxor (shift_left i 1) (shift_right i 63))
end

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

let int_of_element_type = function
  | T_BOOL -> 2
  | T_BYTE | T_I8 -> 3
  | T_I16 -> 4
  | T_I32 -> 5
  | T_I64 -> 6
  | T_DOUBLE -> 7
  | T_STRING | T_BINARY -> 8
  | T_LIST -> 9
  | T_SET -> 10
  | T_MAP -> 11
  | T_STRUCT -> 12

let element_type_of_int = function
  | 2 -> T_BOOL
  | 3 -> T_BYTE
  | 4 -> T_I16
  | 5 -> T_I32
  | 6 -> T_I64
  | 7 -> T_DOUBLE
  | 8 -> T_BINARY
  | 9 -> T_LIST
  | 10 -> T_SET
  | 11 -> T_MAP
  | 12 -> T_STRUCT
  | _i -> failwith (Printf.sprintf "invalid element type identifier %d" _i)

let int_of_field_ty = function
  | T_BOOL -> assert false
  | T_BYTE | T_I8 -> 3
  | T_I16 -> 4
  | T_I32 -> 5
  | T_I64 -> 6
  | T_DOUBLE -> 7
  | T_STRING | T_BINARY -> 8
  | T_LIST -> 9
  | T_SET -> 10
  | T_MAP -> 11
  | T_STRUCT -> 12

let field_ty_of_int = function
  | 1 | 2 -> T_BOOL
  | 3 -> T_BYTE
  | 4 -> T_I16
  | 5 -> T_I32
  | 6 -> T_I64
  | 7 -> T_DOUBLE
  | 8 -> T_BINARY
  | 9 -> T_LIST
  | 10 -> T_SET
  | 11 -> T_MAP
  | 12 -> T_STRUCT
  | _i -> failwith (Printf.sprintf "invalid field type identifier %d" _i)

let write (tr : 'wr transport_write) : 'wr protocol_write =
  (* use for field delta encoding *)
  let prev_field_id = ref 0 in

  let in_bool_field = ref false in
  let bool_field_id = ref 0 in

  let write_i16 wr i = Enc_.zigzag tr wr (Int64.of_int i) in
  let write_i32 wr i = Enc_.zigzag tr wr (Int64.of_int32 i) in
  let write_i64 wr i = Enc_.zigzag tr wr i in
  let write_byte wr c = tr.write_byte wr c in

  let write_binary wr s =
    let n = String.length s in
    write_i32 wr (Int32.of_int n);
    tr.write wr (Bytes.unsafe_of_string s) 0 n
  in

  let write_string = write_binary in

  let write_msg_begin wr name ty seq =
    tr.write_byte wr (Char.unsafe_chr 0x82);
    let b1 = (int_of_message_type ty lsl 5) lor 0b00001 in
    tr.write_byte wr (Char.unsafe_chr b1);
    Enc_.int_as_varint tr wr seq;
    write_string wr name
  in

  let write_msg_end _ = () in
  let write_struct_begin _ _s = () in
  let write_struct_end _ = () in
  let flush wr = tr.flush wr in

  let write_field_begin_ wr ~ty_as_int:(ty_n : int) id =
    let delta = id - !prev_field_id in
    if delta >= 1 && delta <= 15 then (
      (* compact version *)
      let b0 = (delta lsl 4) lor ty_n in
      tr.write_byte wr (Char.unsafe_chr b0)
    ) else (
      let b0 = ty_n in
      tr.write_byte wr (Char.unsafe_chr b0);
      Enc_.zigzag tr wr (Int64.of_int id)
    );
    prev_field_id := id
  in

  let write_field_begin wr _name ty id =
    if ty = T_BOOL then (
      (* do not write yet, wait for the bool *)
      bool_field_id := id;
      in_bool_field := true
    ) else
      write_field_begin_ wr ~ty_as_int:(int_of_field_ty ty) id
  in

  let write_field_end _ = in_bool_field := false in
  let write_field_stop wr = tr.write_byte wr (Char.unsafe_chr 0) in

  let write_list_begin wr ty sz =
    assert (sz >= 0);
    if sz <= 14 then (
      let b0 = (sz lsl 4) lor int_of_element_type ty in
      tr.write_byte wr (Char.unsafe_chr b0)
    ) else (
      (* long form *)
      let b0 = 0b1111_0000 lor int_of_element_type ty in
      tr.write_byte wr (Char.unsafe_chr b0);
      write_i32 wr (Int32.of_int sz)
    )
  in

  let write_list_end _ = () in
  let write_set_begin wr ty sz = write_list_begin wr ty sz in
  let write_set_end _ = () in

  let write_map_begin wr tyk tyv sz =
    if sz = 0 then
      tr.write_byte wr (Char.unsafe_chr 0)
    else (
      write_i32 wr (Int32.of_int sz);
      let bty = (int_of_element_type tyk lsl 4) lor int_of_element_type tyv in
      tr.write_byte wr (Char.unsafe_chr bty)
    )
  in

  let write_map_end _ = () in

  let write_bool wr b =
    if !in_bool_field then (
      let n =
        if b then
          1
        else
          2
      in
      (* write the whole field header *)
      write_field_begin_ wr ~ty_as_int:n !bool_field_id
    ) else (
      let n = int_of_bool b in
      tr.write_byte wr (Char.unsafe_chr n)
    )
  in

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

let read (tr : 'rd transport_read) : 'rd protocol_read =
  let last_field_id = ref 0 in
  let last_bool = ref 0 in

  let read_bool rd : bool =
    match !last_bool with
    | 0 -> Char.code (tr.read_byte rd) <> 0
    | 1 -> true
    | 2 -> false
    | _ -> failwith "expected a bool"
  in

  let read_byte rd : char = tr.read_byte rd in
  let read_i16 rd : int = Int64.to_int (Dec_.zigzag tr rd) in
  let read_i32 rd : int32 = Int64.to_int32 (Dec_.zigzag tr rd) in
  let read_i64 rd : int64 = Dec_.zigzag tr rd in

  let read_double rd : float =
    let i = read_i64 rd in
    Int64.float_of_bits i
  in

  let read_string rd : string =
    let len = read_i32 rd |> Int32.to_int in
    let buf = Bytes.create len in
    really_read tr rd buf 0 len;
    Bytes.unsafe_to_string buf
  in

  let read_binary = read_string in

  let read_msg_begin rd : string * message_type * sequence_number =
    let pid = Char.code @@ tr.read_byte rd in
    if pid <> 0x82 then failwith "invalid protocol id";
    let b1 = Char.code @@ tr.read_byte rd in
    let vv = b1 land 0b11111 in
    if vv <> 1 then failwith (spf "bad version %d for compact protocol" vv);
    let m_ty = message_type_of_int (b1 lsr 5) in
    let seq = Dec_.varint tr rd |> Int64.to_int in
    let name = read_string rd in
    name, m_ty, seq
  in

  let read_msg_end _ = () in
  let read_struct_begin _ = "" in
  let read_struct_end _ = () in

  let read_field_begin rd : string * field_type * field_id =
    let b0 = Char.code (tr.read_byte rd) in
    if b0 = 0 then raise Read_stop_field;

    let ty = b0 land 0b0000_1111 in
    let ty =
      if ty = 1 || ty = 2 then (
        last_bool := ty;
        T_BOOL
      ) else
        field_ty_of_int ty
    in

    (* get field ID, either a delta from previous field ID
       (short form) or a full zigzag int *)
    let f_id = (b0 land 0b1111_0000) lsr 4 in
    let f_id =
      if f_id = 0 then
        Int64.to_int (Dec_.zigzag tr rd)
      else
        f_id + !last_field_id
    in

    last_field_id := f_id;
    "", ty, f_id
  in

  let read_field_end _ = last_bool := 0 in

  let read_list_begin rd : element_type * size =
    let b0 = Char.code @@ tr.read_byte rd in

    let ty = element_type_of_int (b0 land 0b0000_1111) in
    let size =
      let sz = (b0 land 0b1111_0000) lsr 4 in
      if sz = 0b1111 then
        (* long form: size is a varint *)
        Dec_.varint tr rd |> Int64.to_int
      else
        sz
    in
    ty, size
  in

  let read_list_end _ = () in
  let read_set_begin rd : element_type * size = read_list_begin rd in
  let read_set_end _ = () in

  let read_map_begin rd : element_type * element_type * size =
    let b0 = Char.code @@ tr.read_byte rd in
    if b0 = 0 then
      T_BOOL, T_BOOL, 0
    else (
      let sz = Int64.to_int (Dec_.varint tr rd) in
      let kv = Char.code @@ tr.read_byte rd in
      let tyk = element_type_of_int (kv lsr 4) in
      let tyv = element_type_of_int (kv land 0b1111) in
      tyk, tyv, sz
    )
  in
  let read_map_end _ = () in

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

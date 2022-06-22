open Types

let spf = Printf.sprintf

module Dec_ = struct
  let varint (module TR : TRANSPORT_READ) : int64 =
    let shift = ref 0 in
    let res = ref 0L in
    let continue = ref true in
    while !continue do
      let b = Char.code (TR.read_byte ()) in
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

  let zigzag tr : int64 =
    let v = varint tr in
    Int64.(logxor (shift_right v 1) (neg (logand v Int64.one)))
end

module Enc_ = struct
  let varint (module TR : TRANSPORT_WRITE) (i : int64) : unit =
    let i = ref i in
    let continue = ref true in
    while !continue do
      let cur = Int64.(logand !i 0x7fL) in
      if cur = !i then (
        continue := false;
        TR.write_byte (Char.unsafe_chr Int64.(to_int cur))
      ) else (
        TR.write_byte (Char.unsafe_chr Int64.(to_int (logor 0x80L cur)));
        i := Int64.shift_right_logical !i 7
      )
    done

  let int_as_varint tr i = varint tr (Int64.of_int i)

  let zigzag tr (i : int64) =
    varint tr Int64.(logxor (shift_left i 1) (shift_right i 63))
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

let write (tr : transport_write) : protocol_write =
  (* use for field delta encoding *)
  let prev_field_id = ref 0 in

  let in_bool_field = ref false in
  let bool_field_id = ref 0 in

  let (module Tr) = tr in
  let module M = struct
    let write_i16 i = Enc_.zigzag tr (Int64.of_int i)
    let write_i32 i = Enc_.zigzag tr (Int64.of_int32 i)
    let write_i64 i = Enc_.zigzag tr i
    let write_byte c = Tr.write_byte c

    let write_binary s =
      let n = String.length s in
      write_i32 (Int32.of_int n);
      Tr.write (Bytes.unsafe_of_string s) 0 n

    let write_string s = write_binary s

    let write_msg_begin name ty seq =
      Tr.write_byte (Char.unsafe_chr 0x82);
      let b1 = (int_of_message_type ty lsl 5) lor 0b00001 in
      Tr.write_byte (Char.unsafe_chr b1);
      Enc_.int_as_varint tr seq;
      write_string name

    let write_msg_end () = ()
    let write_struct_begin _s = ()
    let write_struct_end () = ()

    let write_field_begin_ ~ty_as_int:(ty_n : int) id =
      let delta = id - !prev_field_id in
      if delta >= 1 && delta <= 15 then (
        (* compact version *)
        let b0 = (delta lsl 4) lor ty_n in
        Tr.write_byte (Char.unsafe_chr b0)
      ) else (
        let b0 = ty_n in
        Tr.write_byte (Char.unsafe_chr b0);
        Enc_.zigzag tr (Int64.of_int id)
      );
      prev_field_id := id

    let write_field_begin _name ty id =
      if ty = T_BOOL then (
        (* do not write yet, wait for the bool *)
        bool_field_id := id;
        in_bool_field := true
      ) else
        write_field_begin_ ~ty_as_int:(int_of_field_ty ty) id

    let write_field_end () = in_bool_field := false
    let write_field_stop () = Tr.write_byte (Char.unsafe_chr 0)

    let write_list_begin ty sz =
      assert (sz >= 0);
      if sz <= 14 then (
        let b0 = (sz lsl 4) lor int_of_element_type ty in
        Tr.write_byte (Char.unsafe_chr b0)
      ) else (
        (* long form *)
        let b0 = 0b1111_0000 lor int_of_element_type ty in
        Tr.write_byte (Char.unsafe_chr b0);
        write_i32 (Int32.of_int sz)
      )

    let write_list_end () = ()
    let write_set_begin ty sz = write_list_begin ty sz
    let write_set_end () = ()

    let write_map_begin tyk tyv sz =
      if sz = 0 then
        Tr.write_byte (Char.unsafe_chr 0)
      else (
        write_i32 (Int32.of_int sz);
        let bty = (int_of_element_type tyk lsl 4) lor int_of_element_type tyv in
        Tr.write_byte (Char.unsafe_chr bty)
      )

    let write_map_end () = ()

    let write_bool b =
      if !in_bool_field then (
        let n =
          if b then
            1
          else
            2
        in
        (* write the whole field header *)
        write_field_begin_ ~ty_as_int:n !bool_field_id
      ) else (
        let n =
          if b then
            1
          else
            0
        in
        Tr.write_byte (Char.unsafe_chr n)
      )

    let write_double f =
      let i = Int64.bits_of_float f in
      write_i64 i
  end in
  (module M)

let read (tr : transport_read) : protocol_read =
  let (module Tr) = tr in

  let last_field_id = ref 0 in
  let last_bool = ref 0 in

  let module M = struct
    let read_bool () : bool =
      match !last_bool with
      | 0 -> Char.code (Tr.read_byte ()) <> 0
      | 1 -> true
      | 2 -> false
      | _ -> failwith "expected a bool"

    let read_byte () : char = Tr.read_byte ()
    let read_i16 () : int = Int64.to_int (Dec_.zigzag tr)
    let read_i32 () : int32 = Int64.to_int32 (Dec_.zigzag tr)
    let read_i64 () : int64 = Dec_.zigzag tr

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
      let pid = Char.code @@ Tr.read_byte () in
      if pid <> 0x82 then failwith "invalid protocol id";
      let b1 = Char.code @@ Tr.read_byte () in
      let vv = b1 land 0b11111 in
      if vv <> 1 then failwith (spf "bad version %d for compact protocol" vv);
      let m_ty = message_type_of_int (b1 lsr 5) in
      let seq = Dec_.varint tr |> Int64.to_int in
      let name = read_string () in
      name, m_ty, seq

    let read_msg_end () = ()
    let read_struct_begin () = ""
    let read_struct_end () = ()

    let read_field_begin () : string * field_type * field_id =
      let b0 = Char.code (Tr.read_byte ()) in
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
          Int64.to_int (Dec_.zigzag tr)
        else
          f_id + !last_field_id
      in

      last_field_id := f_id;
      "", ty, f_id

    let read_field_end () = last_bool := 0

    let read_list_begin () : element_type * size =
      let b0 = Char.code @@ Tr.read_byte () in

      let ty = element_type_of_int (b0 land 0b0000_1111) in
      let size =
        let sz = (b0 land 0b1111_0000) lsr 4 in
        if sz = 0b1111 then
          (* long form: size is a varint *)
          Dec_.varint tr |> Int64.to_int
        else
          sz
      in
      ty, size

    let read_list_end () = ()
    let read_set_begin () : element_type * size = read_list_begin ()
    let read_set_end () = ()

    let read_map_begin () : element_type * element_type * size =
      let b0 = Char.code @@ Tr.read_byte () in
      if b0 = 0 then
        T_BOOL, T_BOOL, 0
      else (
        let sz = Int64.to_int (Dec_.varint tr) in
        let kv = Char.code @@ Tr.read_byte () in
        let tyk = element_type_of_int (kv lsr 4) in
        let tyv = element_type_of_int (kv land 0b1111) in
        tyk, tyv, sz
      )

    let read_map_end () = ()
  end in
  (module M)

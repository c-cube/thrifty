(** Protocol-level debugging utils *)

open Types

module Token = struct
  type t =
    | T_msg_begin of string * message_type * sequence_number
    | T_msg_end
    | T_struct_begin of string
    | T_struct_end
    | T_field_begin of string * field_type * field_id
    | T_field_end
    | T_field_stop
    | T_map_begin of element_type * element_type * size
    | T_map_end
    | T_list_begin of element_type * size
    | T_list_end
    | T_set_begin of element_type * size
    | T_set_end
    | T_bool of bool
    | T_byte of char
    | T_i16 of int
    | T_i32 of int32
    | T_i64 of int64
    | T_double of float
    | T_string of string
    | T_binary of string

  let to_string =
    let spf = Printf.sprintf in
    function
    | T_msg_begin (s, ty, seq) ->
      spf "msg_begin(%S,%s,seq=%d)" s (string_of_message_type ty) seq
    | T_msg_end -> "msg_end"
    | T_struct_begin s -> spf "struct_begin(%S)" s
    | T_struct_end -> "struct_end"
    | T_field_begin (s, ty, i) ->
      spf "field_begin(%S,%s,%d)" s (string_of_field_type ty) i
    | T_field_end -> "field_end"
    | T_field_stop -> "field_stop"
    | T_map_begin (k, v, sz) ->
      spf "map_begin(%d,<%s,%s>)" sz (string_of_element_type k)
        (string_of_element_type v)
    | T_map_end -> "map_end"
    | T_list_begin (ty, sz) ->
      spf "list_begin(%d,%s)" sz (string_of_element_type ty)
    | T_list_end -> "list_end"
    | T_set_begin (ty, sz) ->
      spf "set_begin(%d,%s)" sz (string_of_element_type ty)
    | T_set_end -> "set_end"
    | T_bool b -> spf "%B" b
    | T_byte c -> spf "%C" c
    | T_i16 i -> spf "%d" i
    | T_i32 i -> spf "%ldl" i
    | T_i64 i -> spf "%LdL" i
    | T_double f -> spf "%f" f
    | T_string s -> spf "%S" s
    | T_binary s -> spf "binary(%S)" s

  let pp out self = Format.pp_print_string out (to_string self)
end

(** A write-protocol that produces a stream of tokens *)
let debug_write () =
  let open Token in
  object (self)
    inherit protocol_write
    val mutable toks : Token.t list = []

    method get_tokens : Token.t list = List.rev toks
    (** Obtain the list of tokens emitted *)

    method private add t = toks <- t :: toks
    method write_msg_begin s ty seq = self#add (T_msg_begin (s, ty, seq))
    method write_msg_end = self#add @@ T_msg_end
    method write_struct_begin s = self#add @@ T_struct_begin s
    method write_struct_end = self#add @@ T_struct_end
    method write_field_begin s ty id = self#add @@ T_field_begin (s, ty, id)
    method write_field_end = self#add T_field_end
    method write_field_stop = self#add T_field_stop
    method write_map_begin t1 t2 sz = self#add @@ T_map_begin (t1, t2, sz)
    method write_map_end = self#add T_map_end
    method write_list_begin ty sz = self#add @@ T_list_begin (ty, sz)
    method write_list_end = self#add T_list_end
    method write_set_begin ty sz = self#add @@ T_set_begin (ty, sz)
    method write_set_end = self#add T_set_end
    method write_bool b = self#add @@ T_bool b
    method write_byte x = self#add @@ T_byte x
    method write_i16 x = self#add @@ T_i16 x
    method write_i32 x = self#add @@ T_i32 x
    method write_i64 x = self#add @@ T_i64 x
    method write_double x = self#add @@ T_double x
    method write_string x = self#add @@ T_string x
    method write_binary x = self#add @@ T_binary x
  end

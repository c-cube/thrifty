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

(** A write-protocol that produces a stream of tokens.

   [debug_write()] returns a tuple [f, writer] where [writer]
   is a protocol writer, and [f()] returns the list of tokens
   written so far into [writer]. *)
let write : Token.t list ref protocol_write =
  let open Token in
  let[@inline] add toks t = toks := t :: !toks in
  let write_msg_begin toks s ty seq = add toks (T_msg_begin (s, ty, seq)) in
  let write_msg_end toks = add toks @@ T_msg_end in
  let write_struct_begin toks s = add toks @@ T_struct_begin s in
  let write_struct_end toks = add toks @@ T_struct_end in
  let write_field_begin toks s ty id = add toks @@ T_field_begin (s, ty, id) in
  let write_field_end toks = add toks T_field_end in
  let write_field_stop toks = add toks T_field_stop in
  let write_map_begin toks t1 t2 sz = add toks @@ T_map_begin (t1, t2, sz) in
  let write_map_end toks = add toks T_map_end in
  let write_list_begin toks ty sz = add toks @@ T_list_begin (ty, sz) in
  let write_list_end toks = add toks T_list_end in
  let write_set_begin toks ty sz = add toks @@ T_set_begin (ty, sz) in
  let write_set_end toks = add toks T_set_end in
  let write_bool toks b = add toks @@ T_bool b in
  let write_byte toks x = add toks @@ T_byte x in
  let write_i16 toks x = add toks @@ T_i16 x in
  let write_i32 toks x = add toks @@ T_i32 x in
  let write_i64 toks x = add toks @@ T_i64 x in
  let write_double toks x = add toks @@ T_double x in
  let write_string toks x = add toks @@ T_string x in
  let write_binary toks x = add toks @@ T_binary x in
  let flush _ = () in
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

[@@@ocaml.warning "-8"]

(** [read] is a reader that returns tokens from [toks] one by one. *)
let read : Token.t list ref protocol_read =
  let open Token in
  let push_back toks x = toks := x :: !toks in

  let fail_ msg = raise (Runtime_error (UE_invalid_protocol, msg)) in

  let pop toks =
    match !toks with
    | x :: tl ->
      toks := tl;
      x
    | [] -> fail_ "expected another token"
  in

  (* pop a token matched by [f], else fail with a message mentioning "what" *)
  let pop_a toks what f =
    let tok = pop toks in
    match f tok with
    | x -> x
    | exception Match_failure _ ->
      fail_ (Printf.sprintf "expected %s, got `%s`" what (Token.to_string tok))
  in

  let read_msg_begin toks =
    pop_a toks "msg_begin" (function T_msg_begin (s, ty, seq) -> s, ty, seq)
  in

  let read_msg_end toks = pop_a toks "msg_end" (function T_msg_end -> ()) in

  let read_struct_begin toks =
    pop_a toks "struct begin" (function T_struct_begin s -> s)
  in

  let read_struct_end toks =
    pop_a toks "struct end" (function T_struct_end -> ())
  in

  let read_field_begin toks =
    match pop toks with
    | T_field_begin (s, ty, id) -> s, ty, id
    | T_field_stop -> raise Read_stop_field
    | T_struct_end as tok ->
      (* NOTE: we are permissive here, we handle implicit field_stop *)
      push_back toks tok;
      raise Read_stop_field
    | _tok ->
      fail_
        (Printf.sprintf "expected field_begin, got `%s`" (Token.to_string _tok))
  in

  let read_field_end toks =
    pop_a toks "field end" (function T_field_end -> ())
  in

  let read_map_begin toks =
    pop_a toks "map begin" (function T_map_begin (t1, t2, sz) -> t1, t2, sz)
  in

  let read_map_end toks = pop_a toks "map end" (function T_map_end -> ()) in

  let read_list_begin toks =
    pop_a toks "list begin" (function T_list_begin (ty, sz) -> ty, sz)
  in

  let read_list_end toks =
    pop_a toks "list end" (function T_list_end -> ())
  in

  let read_set_begin toks =
    pop_a toks "set begin" (function T_set_begin (ty, sz) -> ty, sz)
  in

  let read_set_end toks = pop_a toks "set end" (function T_set_end -> ()) in
  let read_bool toks = pop_a toks "bool" (function T_bool b -> b) in
  let read_byte toks = pop_a toks "byte" (function T_byte x -> x) in
  let read_i16 toks = pop_a toks "i16" (function T_i16 x -> x) in
  let read_i32 toks = pop_a toks "i32" (function T_i32 x -> x) in
  let read_i64 toks = pop_a toks "i64" (function T_i64 x -> x) in
  let read_double toks = pop_a toks "double" (function T_double x -> x) in
  let read_string toks = pop_a toks "string" (function T_string x -> x) in
  let read_binary toks = pop_a toks "binary" (function T_binary x -> x) in
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

(** Structured representation of a Thrift value. Useful for debugging. *)
module Value = struct
  type t =
    | V_struct of struct_
    | V_list of element_type * t list
    | V_set of element_type * t list
    | V_map of element_type * element_type * (t * t) list
    | V_bool of bool
    | V_byte of char
    | V_i16 of int
    | V_i32 of int32
    | V_i64 of int64
    | V_double of float
    | V_string of string
    | V_binary of string

  and field = { f_id: field_id; f_ty: field_type; f_v: t }
  and struct_ = { s_name: string; s_fields: field list }

  type msg = {
    name: string;
    ty: message_type;
    seq_num: sequence_number;
    s: struct_;
  }
  (** A message *)

  let rec pp out (self : t) : unit =
    let fpf = Format.fprintf in
    match self with
    | V_bool b -> fpf out "%B" b
    | V_byte c -> fpf out "%C" c
    | V_i16 i -> fpf out "%d" i
    | V_i32 i -> fpf out "%ldl" i
    | V_i64 i -> fpf out "%LdL" i
    | V_double f -> fpf out "%f" f
    | V_string s -> fpf out "%S" s
    | V_binary s -> fpf out "binary(%S)" s
    | V_struct s -> pp_struct out s
    | V_list (ty, l) ->
      fpf out "list %s {@[" (string_of_element_type ty);
      List.iter (fpf out "%a;@ " pp) l;
      fpf out "@]}"
    | V_set (ty, l) ->
      fpf out "set %s {@[" (string_of_element_type ty);
      List.iter (fpf out "%a;@ " pp) l;
      fpf out "@]}"
    | V_map (ty1, ty2, l) ->
      fpf out "map %s %s{@["
        (string_of_element_type ty1)
        (string_of_element_type ty2);
      List.iter (fun (k, v) -> fpf out "@[%a ->@ %a@];@ " pp k pp v) l;
      fpf out "@]}"

  and pp_field out (f : field) =
    Format.fprintf out "@[<2>[%d: %s] = %a@]" f.f_id
      (string_of_field_type f.f_ty)
      pp f.f_v

  and pp_struct out (s : struct_) =
    Format.fprintf out "@[<2>struct %s {@ " s.s_name;
    List.iter (Format.fprintf out "%a;@ " pp_field) s.s_fields;
    Format.fprintf out "@;<1 -2>}@]"

  let pp_msg out (m : msg) : unit =
    Format.fprintf out
      "@[<hv2>msg {@ name=%S;@ ty=%s;@ seq_num=%d@ str=%a@;<1 -2>}@]" m.name
      (string_of_message_type m.ty)
      m.seq_num pp_struct m.s

  let to_tok_list (self : t) : Token.t list =
    let toks = ref [] in
    let add x = toks := x :: !toks in

    let rec aux =
      let open Token in
      function
      | V_bool b -> add @@ T_bool b
      | V_byte c -> add @@ T_byte c
      | V_i16 i -> add @@ T_i16 i
      | V_i32 i -> add @@ T_i32 i
      | V_i64 i -> add @@ T_i64 i
      | V_double f -> add @@ T_double f
      | V_string s -> add @@ T_string s
      | V_binary s -> add @@ T_binary s
      | V_struct s -> aux_struct s
      | V_list (ty, l) ->
        add @@ T_list_begin (ty, List.length l);
        List.iter aux l;
        add @@ T_list_end
      | V_set (ty, l) ->
        add @@ T_set_begin (ty, List.length l);
        List.iter aux l;
        add @@ T_set_end
      | V_map (ty1, ty2, l) ->
        add @@ T_map_begin (ty1, ty2, List.length l);
        List.iter
          (fun (k, v) ->
            aux k;
            aux v)
          l;
        add @@ T_map_end
    and aux_struct s =
      let open Token in
      add @@ T_struct_begin s.s_name;
      List.iter
        (fun f ->
          add @@ T_field_begin ("", f.f_ty, f.f_id);
          aux f.f_v;
          add @@ T_field_end)
        s.s_fields;
      add @@ T_field_stop;
      add @@ T_struct_end
    in

    aux self;
    List.rev !toks

  let msg_to_tok_list (self : msg) : Token.t list =
    let open Token in
    [ T_msg_begin (self.name, self.ty, self.seq_num); T_msg_end ]
    @ to_tok_list (V_struct self.s)
end

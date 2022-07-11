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
let debug_write () : (unit -> Token.t list) * protocol_write =
  let open Token in
  let toks : Token.t list ref = ref [] in
  let get_tokens () = List.rev !toks in
  let[@inline] add t = toks := t :: !toks in
  let module M = struct
    let write_msg_begin s ty seq = add (T_msg_begin (s, ty, seq))
    let write_msg_end () = add @@ T_msg_end
    let write_struct_begin s = add @@ T_struct_begin s
    let write_struct_end () = add @@ T_struct_end
    let write_field_begin s ty id = add @@ T_field_begin (s, ty, id)
    let write_field_end () = add T_field_end
    let write_field_stop () = add T_field_stop
    let write_map_begin t1 t2 sz = add @@ T_map_begin (t1, t2, sz)
    let write_map_end () = add T_map_end
    let write_list_begin ty sz = add @@ T_list_begin (ty, sz)
    let write_list_end () = add T_list_end
    let write_set_begin ty sz = add @@ T_set_begin (ty, sz)
    let write_set_end () = add T_set_end
    let write_bool b = add @@ T_bool b
    let write_byte x = add @@ T_byte x
    let write_i16 x = add @@ T_i16 x
    let write_i32 x = add @@ T_i32 x
    let write_i64 x = add @@ T_i64 x
    let write_double x = add @@ T_double x
    let write_string x = add @@ T_string x
    let write_binary x = add @@ T_binary x
    let flush () = ()
  end in
  get_tokens, (module M)

[@@@ocaml.warning "-8"]

(** [debug_read toks] is a reader that returns tokens from [toks] one by one. *)
let debug_read (toks : Token.t list) : protocol_read =
  let open Token in
  let toks = ref toks in
  let push_back x = toks := x :: !toks in

  let fail_ msg = raise (Runtime_error (UE_invalid_protocol, msg)) in

  let pop () =
    match !toks with
    | x :: tl ->
      toks := tl;
      x
    | [] -> fail_ "expected another token"
  in

  (* pop a token matched by [f], else fail with a message mentioning "what" *)
  let pop_a what f =
    let tok = pop () in
    match f tok with
    | x -> x
    | exception Match_failure _ ->
      fail_ (Printf.sprintf "expected %s, got `%s`" what (Token.to_string tok))
  in

  let module M = struct
    let read_msg_begin () =
      pop_a "msg_begin" (function T_msg_begin (s, ty, seq) -> s, ty, seq)

    let read_msg_end () = pop_a "msg_end" (function T_msg_end -> ())

    let read_struct_begin () =
      pop_a "struct begin" (function T_struct_begin s -> s)

    let read_struct_end () = pop_a "struct end" (function T_struct_end -> ())

    let read_field_begin () =
      match pop () with
      | T_field_begin (s, ty, id) -> s, ty, id
      | T_field_stop -> raise Read_stop_field
      | T_struct_end as tok ->
        (* NOTE: we are permissive here, we handle implicit field_stop *)
        push_back tok;
        raise Read_stop_field
      | _tok ->
        fail_
          (Printf.sprintf "expected field_begin, got `%s`"
             (Token.to_string _tok))

    let read_field_end () = pop_a "field end" (function T_field_end -> ())

    let read_map_begin () =
      pop_a "map begin" (function T_map_begin (t1, t2, sz) -> t1, t2, sz)

    let read_map_end () = pop_a "map end" (function T_map_end -> ())

    let read_list_begin () =
      pop_a "list begin" (function T_list_begin (ty, sz) -> ty, sz)

    let read_list_end () = pop_a "list end" (function T_list_end -> ())

    let read_set_begin () =
      pop_a "set begin" (function T_set_begin (ty, sz) -> ty, sz)

    let read_set_end () = pop_a "set end" (function T_set_end -> ())
    let read_bool () = pop_a "bool" (function T_bool b -> b)
    let read_byte () = pop_a "byte" (function T_byte x -> x)
    let read_i16 () = pop_a "i16" (function T_i16 x -> x)
    let read_i32 () = pop_a "i32" (function T_i32 x -> x)
    let read_i64 () = pop_a "i64" (function T_i64 x -> x)
    let read_double () = pop_a "double" (function T_double x -> x)
    let read_string () = pop_a "string" (function T_string x -> x)
    let read_binary () = pop_a "binary" (function T_binary x -> x)
  end in
  (module M)

[@@@ocaml.warning "+8"]

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

  (* TODO: of_tok_list *)
end

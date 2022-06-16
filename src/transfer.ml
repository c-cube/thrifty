open Types

let transfer_message ((module IP) : protocol_read)
    ((module OP) : protocol_write) : unit =
  let name, msg, seq = IP.read_msg_begin () in
  OP.write_msg_begin name msg seq;
  IP.read_msg_end ();
  OP.write_msg_end ()

let rec transfer_struct ((module IP) : protocol_read)
    ((module OP) : protocol_write) : unit =
  let name = IP.read_struct_begin () in
  OP.write_struct_begin name;

  let rec transfer_ty (ty : field_type) : unit =
    match ty with
    | T_BOOL ->
      let b = IP.read_bool () in
      OP.write_bool b
    | T_BYTE | T_I8 ->
      let c = IP.read_byte () in
      OP.write_byte c
    | T_I16 ->
      let x = IP.read_i16 () in
      OP.write_i16 x
    | T_I32 ->
      let x = IP.read_i32 () in
      OP.write_i32 x
    | T_I64 ->
      let x = IP.read_i64 () in
      OP.write_i64 x
    | T_DOUBLE ->
      let x = IP.read_double () in
      OP.write_double x
    | T_STRING ->
      let x = IP.read_string () in
      OP.write_string x
    | T_BINARY ->
      let x = IP.read_binary () in
      OP.write_binary x
    | T_STRUCT -> transfer_struct (module IP) (module OP)
    | T_LIST ->
      let ty, sz = IP.read_list_begin () in
      OP.write_list_begin ty sz;

      for _i = 1 to sz do
        transfer_ty ty
      done;

      IP.read_list_end ();
      OP.write_list_end ()
    | T_SET ->
      let ty, sz = IP.read_set_begin () in
      OP.write_set_begin ty sz;

      for _i = 1 to sz do
        transfer_ty ty
      done;

      IP.read_set_end ();
      OP.write_set_end ()
    | T_MAP ->
      let ty1, ty2, sz = IP.read_map_begin () in
      OP.write_map_begin ty1 ty2 sz;

      for _i = 1 to sz do
        transfer_ty ty1;
        transfer_ty ty2
      done;

      IP.read_map_end ();
      OP.write_map_end ()
  in

  let continue = ref true in
  while !continue do
    match IP.read_field_begin () with
    | name, ty, field_id ->
      OP.write_field_begin name ty field_id;
      transfer_ty ty;
      IP.read_field_end ();
      OP.write_field_end ()
    | exception Types.Read_stop_field ->
      (* no more fields *)
      continue := false
  done;

  IP.read_struct_end ();
  OP.write_struct_end ()

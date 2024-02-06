open Types

let transfer_message (type rd wr) (pr : rd protocol_read)
    (pw : wr protocol_write) (rd : rd) (wr : wr) : unit =
  let name, msg, seq = pr.read_msg_begin rd in
  pw.write_msg_begin wr name msg seq;
  pr.read_msg_end rd;
  pw.write_msg_end wr

let rec transfer_struct (pr : 'rd protocol_read) (pw : 'wr protocol_write)
    (rd : 'rd) (wr : 'wr) : unit =
  let name = pr.read_struct_begin rd in
  pw.write_struct_begin wr name;

  let rec transfer_ty (ty : field_type) : unit =
    match ty with
    | T_BOOL ->
      let b = pr.read_bool rd in
      pw.write_bool wr b
    | T_BYTE | T_I8 ->
      let c = pr.read_byte rd in
      pw.write_byte wr c
    | T_I16 ->
      let x = pr.read_i16 rd in
      pw.write_i16 wr x
    | T_I32 ->
      let x = pr.read_i32 rd in
      pw.write_i32 wr x
    | T_I64 ->
      let x = pr.read_i64 rd in
      pw.write_i64 wr x
    | T_DOUBLE ->
      let x = pr.read_double rd in
      pw.write_double wr x
    | T_STRING ->
      let x = pr.read_string rd in
      pw.write_string wr x
    | T_BINARY ->
      let x = pr.read_binary rd in
      pw.write_binary wr x
    | T_STRUCT -> transfer_struct pr pw rd wr
    | T_LIST ->
      let ty, sz = pr.read_list_begin rd in
      pw.write_list_begin wr ty sz;

      for _i = 1 to sz do
        transfer_ty ty
      done;

      pr.read_list_end rd;
      pw.write_list_end wr
    | T_SET ->
      let ty, sz = pr.read_set_begin rd in
      pw.write_set_begin wr ty sz;

      for _i = 1 to sz do
        transfer_ty ty
      done;

      pr.read_set_end rd;
      pw.write_set_end wr
    | T_MAP ->
      let ty1, ty2, sz = pr.read_map_begin rd in
      pw.write_map_begin wr ty1 ty2 sz;

      for _i = 1 to sz do
        transfer_ty ty1;
        transfer_ty ty2
      done;

      pr.read_map_end rd;
      pw.write_map_end wr
  in

  let continue = ref true in
  while !continue do
    match pr.read_field_begin rd with
    | name, ty, field_id ->
      pw.write_field_begin wr name ty field_id;
      transfer_ty ty;
      pr.read_field_end rd;
      pw.write_field_end wr
    | exception Types.Read_stop_field ->
      (* no more fields *)
      continue := false;
      pw.write_field_stop wr
  done;

  pr.read_struct_end rd;
  pw.write_struct_end wr

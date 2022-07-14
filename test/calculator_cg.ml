(* generated from "calculator.thrift" using smol_thrift codegen *)
[@@@ocaml.warning {|-26-27-39|}]
let pp_pair ppk ppv out (k,v) = Format.fprintf out {|(%a,%a)|} ppk k ppv v
let pp_list ppx out l = Format.fprintf out {|[@[%a@]]|}
   (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out {|;@ |}) ppx) l
open Thrifty.Types

type intlist = {
  ints: (int32) list
}

let rec pp_intlist out (self:intlist) =
  Format.fprintf out "{@[";
  Format.fprintf out "ints="; pp_list
    (fun out x -> Format.fprintf out "%ld" x) out self.ints;
  Format.fprintf out "@]}"

(** Serialize *)
let rec write_intlist (module OP:PROTOCOL_WRITE) (self:intlist) : unit =
  let {ints} = self in
  OP.write_struct_begin "intlist";
  begin
    begin
      OP.write_field_begin "ints" T_LIST 1;
      OP.write_list_begin T_I32 (List.length ints);
      List.iter (fun x -> OP.write_i32 x) ints;
      OP.write_list_end();
      OP.write_field_end();
    end
  end;
  OP.write_field_stop ();
  OP.write_struct_end ()

(** Deserialize *)
let rec read_intlist (module IP:PROTOCOL_READ) : intlist =
  let _name = IP.read_struct_begin () in
  let continue = ref true in
  let ints = ref None in
  while !continue do
    match IP.read_field_begin () with
    | exception Thrifty.Types.Read_stop_field -> continue := false
    | ("ints", T_LIST, _) | (_, T_LIST, 1) ->
      ints :=
        Some((let _ty, len = IP.read_list_begin () in
              assert (len=0 || match _ty with T_I32 -> true | _ -> false);
              let l = List.init len (fun _i -> IP.read_i32 ()) in
              IP.read_list_end(); l));
    | ("ints", _, _) | (_, _, 1) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (1: "ints")|}))
    | _ -> () (* unknown field *)
  done;
  IP.read_struct_end ();
  let ints = match !ints with
    | None ->
      raise (Runtime_error (UE_invalid_protocol,
               {|field (1: "ints") is required|}))
    | Some x -> x in
  {ints}

exception Div_by_zero

(** Server-side for service "s" *)
class virtual server_s = object (self)
  inherit service_any
  method name = "s"

  method virtual add :
    a:int32-> b:int32 ->  unit -> int32 server_outgoing_reply

  method virtual div :
    a:int32-> b:int32 ->  unit -> int32 server_outgoing_reply

  method virtual add_all : l:intlist ->  unit -> int32 server_outgoing_reply

  method virtual ping :  unit -> unit

  method virtual get_pings :  unit -> int32 server_outgoing_reply

  (** Process an incoming message *)
  method process (ip:protocol_read) ~(reply:(protocol_write -> unit) -> unit) : unit =
    let (module IP) = ip in
    let msg_name, msg_ty, seq_num = IP.read_msg_begin () in
    IP.read_msg_end();
    (* reply using a runtime failure *)
    let reply_exn_ (ue:unexpected_exception) (msg:string) : unit =
      reply @@ fun (module OP:PROTOCOL_WRITE) ->
      OP.write_msg_begin {||} MSG_EXCEPTION seq_num;
      OP.write_msg_end ();
      let ty = Thrifty.Types.int_of_unexpected_exception ue in
      begin
        begin
          OP.write_field_begin "type" T_I32 0;
          OP.write_i32 ty;
          OP.write_field_end();
        end;
        begin
          OP.write_field_begin "message" T_STRING 1;
          OP.write_string msg;
          OP.write_field_end();
        end
      end
    in
    try (
    match msg_name, msg_ty with
    | "add", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let a = ref None in
      let b = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Thrifty.Types.Read_stop_field -> continue := false
        | ("a", T_I32, _) | (_, T_I32, 1) ->
          a := Some(IP.read_i32 ());
        | ("a", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "a")|}))
        | ("b", T_I32, _) | (_, T_I32, 2) ->
          b := Some(IP.read_i32 ());
        | ("b", _, _) | (_, _, 2) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (2: "b")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let a = match !a with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (1: "a") is required|}))
        | Some x -> x in
      let b = match !b with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (2: "b") is required|}))
        | Some x -> x in
      let reply (x:_ result) : unit =
        match x with
        | Ok res ->
          reply @@ fun (module OP:PROTOCOL_WRITE) ->
          OP.write_msg_begin {||} MSG_REPLY seq_num;
          OP.write_msg_end();
          OP.write_struct_begin {||};
          begin
            OP.write_field_begin "success" T_I32 0;
            OP.write_i32 res;
            OP.write_field_end();
          end;
          OP.write_field_stop();
          OP.write_struct_end ();
          OP.flush ()
        | Error exn ->
          raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       in
      (* call the user code *)
      (try self#add ~a ~b () ~reply
       with e -> reply (Error e))
    | "div", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let a = ref None in
      let b = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Thrifty.Types.Read_stop_field -> continue := false
        | ("a", T_I32, _) | (_, T_I32, 1) ->
          a := Some(IP.read_i32 ());
        | ("a", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "a")|}))
        | ("b", T_I32, _) | (_, T_I32, 2) ->
          b := Some(IP.read_i32 ());
        | ("b", _, _) | (_, _, 2) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (2: "b")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let a = match !a with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (1: "a") is required|}))
        | Some x -> x in
      let b = match !b with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (2: "b") is required|}))
        | Some x -> x in
      let reply (x:_ result) : unit =
        match x with
        | Ok res ->
          reply @@ fun (module OP:PROTOCOL_WRITE) ->
          OP.write_msg_begin {||} MSG_REPLY seq_num;
          OP.write_msg_end();
          OP.write_struct_begin {||};
          begin
            OP.write_field_begin "success" T_I32 0;
            OP.write_i32 res;
            OP.write_field_end();
          end;
          OP.write_field_stop();
          OP.write_struct_end ();
          OP.flush ()
        | Error Div_by_zero ->
          reply @@ fun (module OP:PROTOCOL_WRITE) ->
          OP.write_msg_begin {||} MSG_REPLY seq_num;
          OP.write_msg_end();
          OP.write_struct_begin {||};
          OP.write_field_begin {|e|} T_STRUCT 1;
          OP.write_field_stop();
          OP.write_field_end();
          OP.write_field_stop();
          OP.write_struct_end ();
          OP.flush ()
        | Error exn ->
          raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       in
      (* call the user code *)
      (try self#div ~a ~b () ~reply
       with e -> reply (Error e))
    | "add_all", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let l = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Thrifty.Types.Read_stop_field -> continue := false
        | ("l", T_STRUCT, _) | (_, T_STRUCT, 1) ->
          l := Some(read_intlist (module IP) );
        | ("l", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "l")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let l = match !l with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (1: "l") is required|}))
        | Some x -> x in
      let reply (x:_ result) : unit =
        match x with
        | Ok res ->
          reply @@ fun (module OP:PROTOCOL_WRITE) ->
          OP.write_msg_begin {||} MSG_REPLY seq_num;
          OP.write_msg_end();
          OP.write_struct_begin {||};
          begin
            OP.write_field_begin "success" T_I32 0;
            OP.write_i32 res;
            OP.write_field_end();
          end;
          OP.write_field_stop();
          OP.write_struct_end ();
          OP.flush ()
        | Error exn ->
          raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       in
      (* call the user code *)
      (try self#add_all ~l () ~reply
       with e -> reply (Error e))
    | "ping", MSG_ONEWAY ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      while !continue do
        match IP.read_field_begin () with
        | exception Thrifty.Types.Read_stop_field -> continue := false
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      (try self#ping  () with _ -> ());
    | "get_pings", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      while !continue do
        match IP.read_field_begin () with
        | exception Thrifty.Types.Read_stop_field -> continue := false
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let reply (x:_ result) : unit =
        match x with
        | Ok res ->
          reply @@ fun (module OP:PROTOCOL_WRITE) ->
          OP.write_msg_begin {||} MSG_REPLY seq_num;
          OP.write_msg_end();
          OP.write_struct_begin {||};
          begin
            OP.write_field_begin "success" T_I32 0;
            OP.write_i32 res;
            OP.write_field_end();
          end;
          OP.write_field_stop();
          OP.write_struct_end ();
          OP.flush ()
        | Error exn ->
          raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       in
      (* call the user code *)
      (try self#get_pings  () ~reply
       with e -> reply (Error e))
    | _n, _ -> raise (Runtime_error (UE_invalid_message_type, {|invalid message|}));
    ) with Runtime_error (ue, msg) ->
      (* catch runtime errors and reify them *)
      reply_exn_ ue msg;
end

(** Client-side for service "s" *)
module Client_s : sig

  val add : a:int32-> b:int32 ->  int32 client_outgoing_call

  val div : a:int32-> b:int32 ->  int32 client_outgoing_call

  val add_all : l:intlist ->  int32 client_outgoing_call

  val ping :  client_outgoing_oneway

  val get_pings :  int32 client_outgoing_call

end = struct

  let add ~(a:int32) ~(b:int32) : _ client_outgoing_call =
    fun ~seq_num (module OP:PROTOCOL_WRITE) ->
    OP.write_msg_begin "add" MSG_CALL seq_num;
    OP.write_msg_end();
    (* write arguments *)
    OP.write_struct_begin "add";
    begin
      begin
        OP.write_field_begin "a" T_I32 1;
        OP.write_i32 a;
        OP.write_field_end();
      end;
      begin
        OP.write_field_begin "b" T_I32 2;
        OP.write_i32 b;
        OP.write_field_end();
      end
    end;
    OP.write_field_stop ();
    OP.write_struct_end ();
    OP.flush ();
    (* reading the reply, later *)
    let read_reply (module IP:PROTOCOL_READ) : int32 =
      let _name, ty, seq_num' = IP.read_msg_begin () in
      IP.read_msg_end();
      if (seq_num <> seq_num') then
        raise (Runtime_error (UE_bad_sequence_id, {||}));
      match ty with
      | MSG_REPLY ->
        (* expected reply: success or declared exception *)
        let (_:string) = IP.read_struct_begin () in
        let _, ty, f_id =
          try IP.read_field_begin ()
          with Read_stop_field ->
            raise (Runtime_error (UE_missing_result, {|expected a field|})) in
        (match f_id with
         | 0 -> let res = IP.read_i32 () in IP.read_struct_end(); res
         | _id ->
           raise
            (Runtime_error
              (UE_invalid_protocol,
               Printf.sprintf {|unexpected error code %d|} _id)))
      | MSG_EXCEPTION ->
        (* errors raised on the server side *)
        let continue = ref true in
        let ty = ref None in
        let msg = ref None in
        while !continue do
          match IP.read_field_begin () with
          | exception Thrifty.Types.Read_stop_field -> continue := false
          | ("type", T_I32, _) | (_, T_I32, 0) ->
            ty := Some(IP.read_i32 ());
          | ("type", _, _) | (_, _, 0) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (0: "type")|}))
          | ("message", (T_STRING | T_BINARY), _) | (_, (T_STRING | T_BINARY), 1) ->
            msg := Some(IP.read_string ());
          | ("message", _, _) | (_, _, 1) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (1: "message")|}))
          | _ -> () (* unknown field *)
        done;
        let msg = Option.fold ~none:{||} ~some:(fun x->x) !msg in
        let ue = match !ty with
          | None -> UE_unknown
          | Some ty -> unexpected_exception_of_int ty in
        raise (Runtime_error (ue, msg))
      | _ ->
        raise (Runtime_error
               (UE_invalid_protocol, {|expected reply or exception|}))
    in
    (), read_reply

  let div ~(a:int32) ~(b:int32) : _ client_outgoing_call =
    fun ~seq_num (module OP:PROTOCOL_WRITE) ->
    OP.write_msg_begin "div" MSG_CALL seq_num;
    OP.write_msg_end();
    (* write arguments *)
    OP.write_struct_begin "div";
    begin
      begin
        OP.write_field_begin "a" T_I32 1;
        OP.write_i32 a;
        OP.write_field_end();
      end;
      begin
        OP.write_field_begin "b" T_I32 2;
        OP.write_i32 b;
        OP.write_field_end();
      end
    end;
    OP.write_field_stop ();
    OP.write_struct_end ();
    OP.flush ();
    (* reading the reply, later *)
    let read_reply (module IP:PROTOCOL_READ) : int32 =
      let _name, ty, seq_num' = IP.read_msg_begin () in
      IP.read_msg_end();
      if (seq_num <> seq_num') then
        raise (Runtime_error (UE_bad_sequence_id, {||}));
      match ty with
      | MSG_REPLY ->
        (* expected reply: success or declared exception *)
        let (_:string) = IP.read_struct_begin () in
        let _, ty, f_id =
          try IP.read_field_begin ()
          with Read_stop_field ->
            raise (Runtime_error (UE_missing_result, {|expected a field|})) in
        (match f_id with
         | 0 -> let res = IP.read_i32 () in IP.read_struct_end(); res
         | 1 ->
           raise Div_by_zero
         | _id ->
           raise
            (Runtime_error
              (UE_invalid_protocol,
               Printf.sprintf {|unexpected error code %d|} _id)))
      | MSG_EXCEPTION ->
        (* errors raised on the server side *)
        let continue = ref true in
        let ty = ref None in
        let msg = ref None in
        while !continue do
          match IP.read_field_begin () with
          | exception Thrifty.Types.Read_stop_field -> continue := false
          | ("type", T_I32, _) | (_, T_I32, 0) ->
            ty := Some(IP.read_i32 ());
          | ("type", _, _) | (_, _, 0) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (0: "type")|}))
          | ("message", (T_STRING | T_BINARY), _) | (_, (T_STRING | T_BINARY), 1) ->
            msg := Some(IP.read_string ());
          | ("message", _, _) | (_, _, 1) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (1: "message")|}))
          | _ -> () (* unknown field *)
        done;
        let msg = Option.fold ~none:{||} ~some:(fun x->x) !msg in
        let ue = match !ty with
          | None -> UE_unknown
          | Some ty -> unexpected_exception_of_int ty in
        raise (Runtime_error (ue, msg))
      | _ ->
        raise (Runtime_error
               (UE_invalid_protocol, {|expected reply or exception|}))
    in
    (), read_reply

  let add_all ~(l:intlist) : _ client_outgoing_call =
    fun ~seq_num (module OP:PROTOCOL_WRITE) ->
    OP.write_msg_begin "add_all" MSG_CALL seq_num;
    OP.write_msg_end();
    (* write arguments *)
    OP.write_struct_begin "add_all";
    begin
      begin
        OP.write_field_begin "l" T_STRUCT 1;
        write_intlist (module OP) l;
        OP.write_field_end();
      end
    end;
    OP.write_field_stop ();
    OP.write_struct_end ();
    OP.flush ();
    (* reading the reply, later *)
    let read_reply (module IP:PROTOCOL_READ) : int32 =
      let _name, ty, seq_num' = IP.read_msg_begin () in
      IP.read_msg_end();
      if (seq_num <> seq_num') then
        raise (Runtime_error (UE_bad_sequence_id, {||}));
      match ty with
      | MSG_REPLY ->
        (* expected reply: success or declared exception *)
        let (_:string) = IP.read_struct_begin () in
        let _, ty, f_id =
          try IP.read_field_begin ()
          with Read_stop_field ->
            raise (Runtime_error (UE_missing_result, {|expected a field|})) in
        (match f_id with
         | 0 -> let res = IP.read_i32 () in IP.read_struct_end(); res
         | _id ->
           raise
            (Runtime_error
              (UE_invalid_protocol,
               Printf.sprintf {|unexpected error code %d|} _id)))
      | MSG_EXCEPTION ->
        (* errors raised on the server side *)
        let continue = ref true in
        let ty = ref None in
        let msg = ref None in
        while !continue do
          match IP.read_field_begin () with
          | exception Thrifty.Types.Read_stop_field -> continue := false
          | ("type", T_I32, _) | (_, T_I32, 0) ->
            ty := Some(IP.read_i32 ());
          | ("type", _, _) | (_, _, 0) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (0: "type")|}))
          | ("message", (T_STRING | T_BINARY), _) | (_, (T_STRING | T_BINARY), 1) ->
            msg := Some(IP.read_string ());
          | ("message", _, _) | (_, _, 1) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (1: "message")|}))
          | _ -> () (* unknown field *)
        done;
        let msg = Option.fold ~none:{||} ~some:(fun x->x) !msg in
        let ue = match !ty with
          | None -> UE_unknown
          | Some ty -> unexpected_exception_of_int ty in
        raise (Runtime_error (ue, msg))
      | _ ->
        raise (Runtime_error
               (UE_invalid_protocol, {|expected reply or exception|}))
    in
    (), read_reply

  let ping  : client_outgoing_oneway =
    fun ~seq_num (module OP:PROTOCOL_WRITE) ->
    OP.write_msg_begin "ping" MSG_ONEWAY seq_num;
    OP.write_msg_end();
    (* write arguments *)
    OP.write_struct_begin "ping";
    begin
      
    end;
    OP.write_field_stop ();
    OP.write_struct_end ();
    OP.flush ();
    ()

  let get_pings  : _ client_outgoing_call =
    fun ~seq_num (module OP:PROTOCOL_WRITE) ->
    OP.write_msg_begin "get_pings" MSG_CALL seq_num;
    OP.write_msg_end();
    (* write arguments *)
    OP.write_struct_begin "get_pings";
    begin
      
    end;
    OP.write_field_stop ();
    OP.write_struct_end ();
    OP.flush ();
    (* reading the reply, later *)
    let read_reply (module IP:PROTOCOL_READ) : int32 =
      let _name, ty, seq_num' = IP.read_msg_begin () in
      IP.read_msg_end();
      if (seq_num <> seq_num') then
        raise (Runtime_error (UE_bad_sequence_id, {||}));
      match ty with
      | MSG_REPLY ->
        (* expected reply: success or declared exception *)
        let (_:string) = IP.read_struct_begin () in
        let _, ty, f_id =
          try IP.read_field_begin ()
          with Read_stop_field ->
            raise (Runtime_error (UE_missing_result, {|expected a field|})) in
        (match f_id with
         | 0 -> let res = IP.read_i32 () in IP.read_struct_end(); res
         | _id ->
           raise
            (Runtime_error
              (UE_invalid_protocol,
               Printf.sprintf {|unexpected error code %d|} _id)))
      | MSG_EXCEPTION ->
        (* errors raised on the server side *)
        let continue = ref true in
        let ty = ref None in
        let msg = ref None in
        while !continue do
          match IP.read_field_begin () with
          | exception Thrifty.Types.Read_stop_field -> continue := false
          | ("type", T_I32, _) | (_, T_I32, 0) ->
            ty := Some(IP.read_i32 ());
          | ("type", _, _) | (_, _, 0) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (0: "type")|}))
          | ("message", (T_STRING | T_BINARY), _) | (_, (T_STRING | T_BINARY), 1) ->
            msg := Some(IP.read_string ());
          | ("message", _, _) | (_, _, 1) ->
            raise (Runtime_error
              (UE_invalid_protocol, {|invalid type for field (1: "message")|}))
          | _ -> () (* unknown field *)
        done;
        let msg = Option.fold ~none:{||} ~some:(fun x->x) !msg in
        let ue = match !ty with
          | None -> UE_unknown
          | Some ty -> unexpected_exception_of_int ty in
        raise (Runtime_error (ue, msg))
      | _ ->
        raise (Runtime_error
               (UE_invalid_protocol, {|expected reply or exception|}))
    in
    (), read_reply

end


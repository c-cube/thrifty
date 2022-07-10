(** Direct (blocking) call to services.

    This is useful for testing purposes, mostly. Calls are immediately
    processed by the service, and reserialized for the caller. *)

open Types

let call_with_reply (s : service_any) (c : 'a client_outgoing_call) : 'a =
  let module BT = Basic_transports in
  let seq_num = 0 in
  let buf = Buffer.create 32 in
  let tr_w = BT.transport_of_buffer buf in
  let w = Binary_protocol.write tr_w in
  let (), read_reply = c ~seq_num w in
  let call_str = Buffer.contents buf in
  Buffer.clear buf;
  (*Format.printf "call: %S@." call_str;*)
  (* call service *)
  s#process
    (Binary_protocol.read @@ BT.transport_of_string call_str)
    ~reply:(fun f -> f w);
  let reply_str = Buffer.contents buf in
  (*Format.printf "reply: %S@." call_str;*)
  let res =
    read_reply (Binary_protocol.read @@ BT.transport_of_string reply_str)
  in
  res

let call_oneway (s : service_any) (c : client_outgoing_oneway) : unit =
  let module BT = Basic_transports in
  let seq_num = 0 in
  let buf = Buffer.create 32 in
  let tr_w = BT.transport_of_buffer buf in
  let w = Binary_protocol.write tr_w in
  let () = c ~seq_num w in
  let call_str = Buffer.contents buf in
  Buffer.clear buf;
  (* call service *)
  s#process
    (Binary_protocol.read @@ BT.transport_of_string call_str)
    ~reply:(fun _f -> ());
  ()

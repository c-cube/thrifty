(** Direct (blocking) call to services.

    This is useful for testing purposes, mostly. Calls are immediately
    processed by the service, and reserialized for the caller. *)

open Types

let call_with_reply (s : service) (c : 'a client_outgoing_call) : 'a =
  let seq_num = 0 in
  let buf = Buffer.create 32 in
  let proto_wr = BinaryProtocol.write TBuffer.write in
  let (), read_reply = c ~seq_num (PR_write (proto_wr, buf)) in
  let call_str = Buffer.contents buf in
  Buffer.clear buf;
  (*Format.printf "call: %S@." call_str;*)
  (* call service *)
  s.process (BinaryProtocol.read @@ TString.read) proto_wr
    (TString.create_reader call_str) ~reply:(fun f -> f buf);
  let reply_str = Buffer.contents buf in
  (*Format.printf "reply: %S@." call_str;*)
  let res =
    read_reply
      (PR_read
         (BinaryProtocol.read TString.read, TString.create_reader reply_str))
  in
  res

let call_oneway (s : service) (c : client_outgoing_oneway) : unit =
  let seq_num = 0 in
  let buf = Buffer.create 32 in
  let tr_w = TBuffer.write in
  let pw = BinaryProtocol.write tr_w in
  let () = c ~seq_num (PR_write (pw, buf)) in
  let call_str = Buffer.contents buf in
  Buffer.clear buf;
  (* call service *)
  s.process (BinaryProtocol.read TString.read) pw
    (TString.create_reader call_str) ~reply:(fun _f -> ());
  ()

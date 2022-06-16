(** Thrift runtime interface *)

module Types = Types
module Basic_transports = Basic_transports
module Debug_protocol = Debug_protocol
module Binary_protocol = Binary_protocol
module Service_multiplex = Service_multiplex
module Transfer = Transfer
open Types

(** Encode to string using a protocol and encoder.
    @param protocol a function that writes to a transport using
    a given protocol (default is the binary protocol) *)
let write_to_string ?(protocol = Binary_protocol.write)
    (enc : protocol_write -> 'a -> unit) (x : 'a) : string =
  let buf = Buffer.create 32 in
  let transport = Basic_transports.transport_of_buffer buf in
  let op = protocol transport in
  enc op x;
  let (module Tr) = transport in
  Tr.flush ();
  Buffer.contents buf

(** Decode a string using a protocol and decoder.
    @param protocol function that read structured data from a transport (byte stream).
    Default is the binary protocol. *)
let read_from_string ?(protocol = Binary_protocol.read)
    (dec : protocol_read -> 'a) (str : string) : 'a =
  let transport = Basic_transports.transport_of_string str in
  let ip = protocol transport in
  let x = dec ip in
  x

(* TODO: a forwarder from protocol_read to protocol_write *)

(* TODO: a unix interface with TCP client/server *)

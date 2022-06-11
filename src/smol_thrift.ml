(** Thrift runtime interface *)

module Types = Types
module Basic_transports = Basic_transports
module Debug_protocol = Debug_protocol
module Binary_protocol = Binary_protocol
module Service_multiplex = Service_multiplex

(* TODO: a forwarder from protocol_read to protocol_write *)

(* TODO: a unix interface with TCP client/server *)

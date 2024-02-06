(** Thrift runtime interface *)

module Types = Types
module TBuffer = TBuffer
module TChannel = TChannel
module TString = TString
module TFile = TFile
module BinaryProtocol = BinaryProtocol
module Debug = Debug
module CompactProtocol = CompactProtocol
module ServiceMultiplex = ServiceMultiplex
module Transfer = Transfer
module Direct_call = Direct_call
open Types

(** Encode to string using a protocol and encoder.
    @param protocol a function that writes to a transport using
    a given protocol (default is the binary protocol) *)
let write_to_string ?(protocol = BinaryProtocol.write)
    (enc : 'wr protocol_write -> 'wr -> 'a -> unit) (x : 'a) : string =
  let buf = Buffer.create 32 in
  let transport = TBuffer.write in
  let op = protocol transport in
  enc op buf x;
  transport.flush buf;
  Buffer.contents buf

(** Decode a string using a protocol and decoder.
    @param protocol function that read structured data from a transport (byte stream).
    Default is the binary protocol. *)
let read_from_string ?(protocol = BinaryProtocol.read)
    (dec : 'rd protocol_read -> 'rd -> 'a) (str : string) : 'a =
  let transport = TString.read in
  let rd = TString.create_reader str in
  let pr = protocol transport in
  let x = dec pr rd in
  x

(* TODO: a unix interface with TCP client/server *)

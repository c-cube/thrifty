open Types

module TBuffer : sig
  val write : Buffer.t transport_write
  (** A transport that collects data into a buffer *)
end

module TString : sig
  type reader
  (** Transport that reads from a string *)

  val read : reader transport_read
  val create_reader : string -> reader
  val read_any : string -> transport_read_any
end

module TChannel : sig
  val write : out_channel transport_write
  val read : in_channel transport_read

  val with_write_file : string -> (transport_write_any -> 'a) -> 'a
  (** Transport that writes into a file *)

  val with_read_file : string -> (transport_read_any -> 'a) -> 'a
  (** Transport that reads from a file *)
end

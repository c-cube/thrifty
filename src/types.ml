(** Types for Thrift. *)

(** Runtime types *)

type field_type =
  | T_BOOL
  | T_BYTE
  | T_I8
  | T_I16
  | T_I32
  | T_I64
  | T_DOUBLE
  | T_STRING
  | T_BINARY
  | T_STRUCT
  | T_MAP
  | T_SET
  | T_LIST

type element_type = field_type

let string_of_field_type = function
  | T_BOOL -> "BOOL"
  | T_BYTE -> "BYTE"
  | T_I8 -> "I8"
  | T_I16 -> "I16"
  | T_I32 -> "I32"
  | T_I64 -> "I64"
  | T_DOUBLE -> "DOUBLE"
  | T_STRING -> "STRING"
  | T_BINARY -> "BINARY"
  | T_STRUCT -> "STRUCT"
  | T_MAP -> "MAP"
  | T_SET -> "SET"
  | T_LIST -> "LIST"

let string_of_element_type = string_of_field_type

(** Type of a message.

    This indicates the role of the message in the RPC protocol. *)
type message_type = MSG_CALL | MSG_REPLY | MSG_EXCEPTION | MSG_ONEWAY

let string_of_message_type = function
  | MSG_CALL -> "MSG_CALL"
  | MSG_REPLY -> "MSG_REPLY"
  | MSG_EXCEPTION -> "MSG_EXCEPTION"
  | MSG_ONEWAY -> "MSG_ONEWAY"

(** Inspired from the java cases for errors not declared in the IDL
    (see "thrift-rpc.md") *)
type unexpected_exception =
  | UE_unknown  (** used in case the type from the peer is unknown. *)
  | UE_unknown_method
      (** used in case the method requested by the client is unknown by the server. *)
  | UE_invalid_message_type  (** no usage was found. *)
  | UE_wrong_method_name  (** no usage was found. *)
  | UE_bad_sequence_id
      (** used internally by the client to indicate a wrong sequence id in the response. *)
  | UE_missing_result
      (** used internally by the client to indicate a response without any field (result nor exception). *)
  | UE_internal_error
      (** used when the server throws an exception that is not declared in the Thrift IDL file.  *)
  | UE_protocol_error
      (** used when something goes wrong during decoding. For example when a list is too long or a required field is missing. *)
  | UE_invalid_transform  (** no usage was found. *)
  | UE_invalid_protocol  (** no usage was found. *)
  | UE_unsupported_client_type  (** no usage was found. *)

let int_of_unexpected_exception = function
  | UE_unknown -> 0l
  | UE_unknown_method -> 1l
  | UE_invalid_message_type -> 2l
  | UE_wrong_method_name -> 3l
  | UE_bad_sequence_id -> 4l
  | UE_missing_result -> 5l
  | UE_internal_error -> 6l
  | UE_protocol_error -> 7l
  | UE_invalid_transform -> 8l
  | UE_invalid_protocol -> 9l
  | UE_unsupported_client_type -> 10l

let unexpected_exception_of_int = function
  | 0l -> UE_unknown
  | 1l -> UE_unknown_method
  | 2l -> UE_invalid_message_type
  | 3l -> UE_wrong_method_name
  | 4l -> UE_bad_sequence_id
  | 5l -> UE_missing_result
  | 6l -> UE_internal_error
  | 7l -> UE_protocol_error
  | 8l -> UE_invalid_transform
  | 9l -> UE_invalid_protocol
  | 10l -> UE_unsupported_client_type
  | _ -> UE_unknown

exception Runtime_error of unexpected_exception * string

type size = int
(** Number of elements in a list/set/map *)

type field_id = int
(** Integer identifier for a field, unique within a struct/union/exception *)

type sequence_number = int
(** Sequence number of a message on the wire. It should be unique in
  a given client/server pair. *)

(** {2 Transports}

    a Transport is concerned with moving bytes from a place to another,
    and possibly handle framing, authentication, etc. It does not care
    what is contained in these bytes. *)

module type TRANSPORT_READ = sig
  val is_closed : unit -> bool
  val close : unit -> unit
  val read_byte : unit -> char
  val read : bytes -> int -> int -> int
end

type transport_read = (module TRANSPORT_READ)
(** Transport to read values *)

(** Really read [n] bytes into [b] at offset [i].
     @raise End_of_file if the input is exhausted first. *)
let really_read ((module R) : transport_read) b i n : unit =
  let i = ref i in
  let n = ref n in
  while !n > 0 do
    let len = R.read b !i !n in
    if len = 0 then raise End_of_file;
    i := !i + len;
    n := !n - len
  done

(** Transport to emit values *)
module type TRANSPORT_WRITE = sig
  val is_closed : unit -> bool
  val close : unit -> unit
  val write_byte : char -> unit
  val write : bytes -> int -> int -> unit
  val flush : unit -> unit
end

type transport_write = (module TRANSPORT_WRITE)

(** {2 Protocols}

    A protocol is a particular encoding/decoding format that specifies
    how to map structured data (the messages and structures and types
    described by the user in the IDL) to bytes.

    A given message can thus be serialized to bytes in various ways by just
    picking a different protocol.
*)

(** Protocol to write messages *)
module type PROTOCOL_WRITE = sig
  val write_msg_begin : string -> message_type -> sequence_number -> unit
  val write_msg_end : unit -> unit
  val write_struct_begin : string -> unit
  val write_struct_end : unit -> unit
  val write_field_begin : string -> field_type -> field_id -> unit
  val write_field_end : unit -> unit

  val write_field_stop : unit -> unit
  (** Indicate that the struct is done, no more fields will be added to it *)

  val write_map_begin : field_type -> field_type -> size -> unit
  val write_map_end : unit -> unit
  val write_list_begin : field_type -> size -> unit
  val write_list_end : unit -> unit
  val write_set_begin : field_type -> size -> unit
  val write_set_end : unit -> unit
  val write_bool : bool -> unit
  val write_byte : char -> unit
  val write_i16 : int -> unit
  val write_i32 : int32 -> unit
  val write_i64 : int64 -> unit
  val write_double : float -> unit
  val write_string : string -> unit
  val write_binary : string -> unit

  val flush : unit -> unit
  (** Flush underlying transport *)
end

type protocol_write = (module PROTOCOL_WRITE)

exception Read_stop_field

(** Protocol to read messages *)
module type PROTOCOL_READ = sig
  val read_msg_begin : unit -> string * message_type * sequence_number
  val read_msg_end : unit -> unit
  val read_struct_begin : unit -> string
  val read_struct_end : unit -> unit

  val read_field_begin : unit -> string * field_type * field_id
  (** Read the next field.
        @raise Read_stop_field if there are no more fields to be read in
        that struct/exception/message *)

  val read_field_end : unit -> unit
  val read_map_begin : unit -> element_type * element_type * size
  val read_map_end : unit -> unit
  val read_list_begin : unit -> element_type * size
  val read_list_end : unit -> unit
  val read_set_begin : unit -> element_type * size
  val read_set_end : unit -> unit
  val read_bool : unit -> bool
  val read_byte : unit -> char
  val read_i16 : unit -> int
  val read_i32 : unit -> int32
  val read_i64 : unit -> int64
  val read_double : unit -> float
  val read_string : unit -> string
  val read_binary : unit -> string
end

type protocol_read = (module PROTOCOL_READ)

(** A pair of read and write builders for a given protocol *)
module type PROTOCOL = sig
  val read : transport_read -> protocol_read
  val write : transport_write -> protocol_write
end

type protocol = (module PROTOCOL)

(** {2 Service-related types} *)

type 'res client_outgoing_call =
  seq_num:sequence_number -> protocol_write -> unit * (protocol_read -> 'res)
(** RPC 2-way call from a client, writing the serialized request into
    the [protocol_write], and return a function to read back
    the answer when it comes back.

    This allows for an asynchronous implementation.
*)

type client_outgoing_oneway = seq_num:sequence_number -> protocol_write -> unit
(** An outgoing oneway RPC call. No response expected. *)

type 'res server_outgoing_reply = reply:(('res, exn) result -> unit) -> unit

(** Class for server implementation of a server. *)
class virtual service_any =
  object
    method virtual process
        : protocol_read -> reply:((protocol_write -> unit) -> unit) -> unit
    (** Process a message. The function is given a [reply] callback
        that it can call when the response is ready. This allows the
        implementation to use a thread pool or an asynchronous framework.

        This might be provided with a different pair of protocols
        every time it is called. *)

    method virtual name : string
    (** Name of this service. This can be useful to multiplex. *)
  end

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

type 'st transport_read = {
  close: 'st -> unit;
  read_byte: 'st -> char;
  read: 'st -> bytes -> int -> int -> int;
}
(** Transport to read a stream of bytes *)

type transport_read_any =
  | TR_read : 'st transport_read * 'st -> transport_read_any

(** Really read [n] bytes into [b] at offset [i].
     @raise End_of_file if the input is exhausted first. *)
let really_read (tr : 'rd transport_read) (read : 'rd) b i n : unit =
  let i = ref i in
  let n = ref n in
  while !n > 0 do
    let len = tr.read read b !i !n in
    if len = 0 then raise End_of_file;
    i := !i + len;
    n := !n - len
  done

type 'st transport_write = {
  close: 'st -> unit;
  write_byte: 'st -> char -> unit;
  write: 'st -> bytes -> int -> int -> unit;
  flush: 'st -> unit;
}
(** Transport to emit values *)

type transport_write_any =
  | TR_write : 'st transport_write * 'st -> transport_write_any

(** {2 Protocols}

    A protocol is a particular encoding/decoding format that specifies
    how to map structured data (the messages and structures and types
    described by the user in the IDL) to bytes.

    A given message can thus be serialized to bytes in various ways by just
    picking a different protocol.
*)

type 'st protocol_write = {
  write_msg_begin: 'st -> string -> message_type -> sequence_number -> unit;
  write_msg_end: 'st -> unit;
  write_struct_begin: 'st -> string -> unit;
  write_struct_end: 'st -> unit;
  write_field_begin: 'st -> string -> field_type -> field_id -> unit;
  write_field_end: 'st -> unit;
  write_field_stop: 'st -> unit;
      (** Indicate that the struct is done, no more fields will be added to it *)
  write_map_begin: 'st -> field_type -> field_type -> size -> unit;
  write_map_end: 'st -> unit;
  write_list_begin: 'st -> field_type -> size -> unit;
  write_list_end: 'st -> unit;
  write_set_begin: 'st -> field_type -> size -> unit;
  write_set_end: 'st -> unit;
  write_bool: 'st -> bool -> unit;
  write_byte: 'st -> char -> unit;
  write_i16: 'st -> int -> unit;
  write_i32: 'st -> int32 -> unit;
  write_i64: 'st -> int64 -> unit;
  write_double: 'st -> float -> unit;
  write_string: 'st -> string -> unit;
  write_binary: 'st -> string -> unit;
  flush: 'st -> unit;  (** Flush underlying transport *)
}
(** Protocol to write messages *)

type protocol_write_any =
  | PR_write : 'a protocol_write * 'a -> protocol_write_any

exception Read_stop_field

type 'st protocol_read = {
  read_msg_begin: 'st -> string * message_type * sequence_number;
  read_msg_end: 'st -> unit;
  read_struct_begin: 'st -> string;
  read_struct_end: 'st -> unit;
  read_field_begin: 'st -> string * field_type * field_id;
      (** Read the next field.
        @raise Read_stop_field if there are no more fields to be read in
        that struct/exception/message *)
  read_field_end: 'st -> unit;
  read_map_begin: 'st -> element_type * element_type * size;
  read_map_end: 'st -> unit;
  read_list_begin: 'st -> element_type * size;
  read_list_end: 'st -> unit;
  read_set_begin: 'st -> element_type * size;
  read_set_end: 'st -> unit;
  read_bool: 'st -> bool;
  read_byte: 'st -> char;
  read_i16: 'st -> int;
  read_i32: 'st -> int32;
  read_i64: 'st -> int64;
  read_double: 'st -> float;
  read_string: 'st -> string;
  read_binary: 'st -> string;
}
(** Protocol to read messages *)

type protocol_read_any = PR_read : 'a protocol_read * 'a -> protocol_read_any

type protocol = {
  read: 'read. 'read transport_read -> 'read protocol_read;
  write: 'write. 'write transport_write -> 'write protocol_write;
}
(** A pair of read and write builders for a given protocol *)

(** {2 Service-related types} *)

type 'res client_outgoing_call =
  seq_num:sequence_number ->
  protocol_write_any ->
  unit * (protocol_read_any -> 'res)
(** RPC 2-way call from a client, writing the serialized request into
    the [protocol_write], and return a function to read back
    the answer when it comes back.

    This allows for an asynchronous implementation.
*)

type client_outgoing_oneway =
  seq_num:sequence_number -> protocol_write_any -> unit
(** An outgoing oneway RPC call. No response expected. *)

type 'res server_outgoing_reply = reply:(('res, exn) result -> unit) -> unit

(** Class for server implementation of a server. *)
class type service_any =
  object
    method process :
      'read 'write.
      'read protocol_read ->
      'write protocol_write ->
      'read ->
      reply:(('write -> unit) -> unit) ->
      unit
    (** Process a message. The function is given a [reply] callback
        that it can call when the response is ready. This allows the
        implementation to use a thread pool or an asynchronous framework.

        This might be provided with a different pair of protocols
        every time it is called. *)

    method name : string
    (** Name of this service. This can be useful to multiplex. *)
  end

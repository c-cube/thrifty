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

(** Type of data found inside a field (or a list/set/map) *)
type field_data = I8 | I16 | I32 | I64 | DOUBLE | STRING | BINARY

let string_of_field_data = function
  | I8 -> "I8"
  | I16 -> "I16"
  | I32 -> "I32"
  | I64 -> "I64"
  | DOUBLE -> "DOUBLE"
  | STRING -> "STRING"
  | BINARY -> "BINARY"

(** Type of a message.

    This indicates the role of the message in the RPC protocol. *)
type message_type = MSG_CALL | MSG_REPLY | MSG_EXCEPTION | MSG_ONEWAY

let string_of_message_type = function
  | MSG_CALL -> "MSG_CALL"
  | MSG_REPLY -> "MSG_REPLY"
  | MSG_EXCEPTION -> "MSG_EXCEPTION"
  | MSG_ONEWAY -> "MSG_ONEWAY"

type size = int
(** Number of elements in a list/set/map *)

type field_id = int
(** Integer identifier for a field, unique within a struct/union/exception *)

type sequence_number = int
(** Sequence number of a message on the wire. It should be unique in
  a given client/server pair. *)

class virtual protocol_write =
  object
    method virtual write_msg_begin
        : string -> message_type -> sequence_number -> unit

    method virtual write_msg_end : unit
    method virtual write_struct_begin : string -> unit
    method virtual write_struct_end : unit
    method virtual write_field_begin : string -> field_type -> field_id -> unit
    method virtual write_field_end : unit

    method virtual write_field_stop : unit
    (** Indicate that the struct is done, no more fields will be added to it *)

    method virtual write_map_begin : field_data -> field_data -> size -> unit
    method virtual write_map_end : unit
    method virtual write_list_begin : field_data -> size -> unit
    method virtual write_list_end : unit
    method virtual write_set_begin : field_data -> size -> unit
    method virtual write_set_end : unit
    method virtual write_bool : bool -> unit
    method virtual write_byte : char -> unit
    method virtual write_i16 : int -> unit
    method virtual write_i32 : int32 -> unit
    method virtual write_i64 : int64 -> unit
    method virtual write_double : float -> unit
    method virtual write_string : string -> unit
    method virtual write_binary : string -> unit
  end

exception Read_stop_field

(** Protocol to read messages *)
class virtual protocol_read =
  object
    method virtual read_msg_begin : string * message_type * sequence_number
    method virtual read_msg_end : unit
    method virtual read_struct_begin : string
    method virtual read_struct_end : unit

    method virtual read_field_begin : string * field_type * field_id
    (** Read the next field.
        @raise Read_stop_field if there are no more fields to be read in
        that struct/exception/message *)

    method virtual read_field_end : unit
    method virtual read_map_begin : field_data * field_data * size
    method virtual read_map_end : unit
    method virtual read_list_begin : field_data * size
    method virtual read_list_end : unit
    method virtual read_set_begin : field_data * size
    method virtual read_set_end : unit
    method virtual read_bool : bool
    method virtual read_byte : char
    method virtual read_i16 : int
    method virtual read_i32 : int32
    method virtual read_i64 : int64
    method virtual read_double : float
    method virtual read_string : string
    method virtual read_binary : string
  end

(** Transport to read values *)
class virtual transport_read =
  object
    method virtual is_closed : bool
    method virtual close : unit
    method virtual read : bytes -> int -> int -> int
  end

(** Transport to emit values *)
class virtual transport_write =
  object
    method virtual is_closed : bool
    method virtual close : unit
    method virtual write : bytes -> int -> int -> unit
    method virtual flush : unit
  end

(** Bidirectional transport *)
class virtual transport =
  object
    inherit transport_read
    inherit transport_write
  end

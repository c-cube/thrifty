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

type size = int
(** Number of elements in a list/set/map *)

type field_id = int
(** Integer identifier for a field, unique within a struct/union/exception *)

type sequence_number = int
(** Sequence number of a message on the wire. It should be unique in
  a given client/server pair. *)

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

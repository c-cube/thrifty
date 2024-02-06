open Types

type reader
(** Transport that reads from a string *)

val read : reader transport_read
val create_reader : string -> reader
val read_any : string -> transport_read_any

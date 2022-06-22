open Types

val transport_of_buffer : Buffer.t -> transport_write
(** A transport that collects data into a buffer *)

val transport_of_string : string -> transport_read
(** Transport that reads from a string *)

val transport_write_file : string -> transport_write
(** Transport that writes into a file *)

val transport_read_file : string -> transport_read
(** Transport that reads from a file *)

val with_transport_write_file : string -> (transport_write -> 'a) -> 'a
val with_transport_read_file : string -> (transport_read -> 'a) -> 'a

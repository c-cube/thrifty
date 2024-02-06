open Types

val with_write_file : string -> (transport_write_any -> 'a) -> 'a
(** Transport that writes into a file *)

val with_read_file : string -> (transport_read_any -> 'a) -> 'a
(** Transport that reads from a file *)

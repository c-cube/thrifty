(** Transfer.

    Transfer data from a {!protocol_read} into a {!protocol_write}
*)

open Types

val transfer_struct :
  'rd protocol_read -> 'wr protocol_write -> 'rd -> 'wr -> unit
(** [transfer_struct r w] reads a struct from [r] (any shape) and
    writes it into [w]. *)

val transfer_message :
  'rd protocol_read -> 'wr protocol_write -> 'rd -> 'wr -> unit
(** [transfer_message r w] reads a message from [r] (any shape) and
    writes it into [w]. *)

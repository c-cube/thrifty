(** Transfer.

    Transfer data from a {!protocol_read} into a {!protocol_write}
*)

open Types

val transfer_struct : protocol_read -> protocol_write -> unit
(** [transfer_struct r w] reads a struct from [r] (any shape) and
    writes it into [w]. *)

val transfer_message : protocol_read -> protocol_write -> unit
(** [transfer_message r w] reads a message from [r] (any shape) and
    writes it into [w]. *)

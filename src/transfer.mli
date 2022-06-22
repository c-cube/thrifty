(** Transfer.

    Transfer data from a {!protocol_read} into a {!protocol_write}
    *)

open Types

val transfer_struct : protocol_read -> protocol_write -> unit
val transfer_message : protocol_read -> protocol_write -> unit

open Types

val write : 'wr transport_write -> 'wr protocol_write
val read : 'rd transport_read -> 'rd protocol_read
val protocol : protocol

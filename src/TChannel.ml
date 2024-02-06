open Types

let write : out_channel transport_write =
  { close = close_out_noerr; write_byte = output_char; write = output; flush }

let read : in_channel transport_read =
  { close = close_in_noerr; read = input; read_byte = input_char }

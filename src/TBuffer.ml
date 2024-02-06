open Types

let write : Buffer.t transport_write =
  let close _buf = () in
  let write_byte buf c = Buffer.add_char buf c in
  let write buf s i len = Buffer.add_subbytes buf s i len in
  let flush _buf = () in
  { close; write_byte; write; flush }

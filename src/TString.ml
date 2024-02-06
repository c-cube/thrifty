open Types

type reader = { s: string; mutable off: int }

let create_reader s : reader = { s; off = 0 }

let read : reader transport_read =
  let close _ = () in

  let read_byte (self : reader) =
    let c = self.s.[self.off] in
    self.off <- self.off + 1;
    c
  in

  let read self buf i len =
    let len = min len (String.length self.s - self.off) in
    Bytes.blit_string self.s self.off buf i len;
    self.off <- self.off + len;
    len
  in
  { close; read_byte; read }

let read_any s = TR_read (read, create_reader s)

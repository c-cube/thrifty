(** Basic transports *)

open Common_
open Types

module TBuffer = struct
  let write : Buffer.t transport_write =
    let close _buf = () in
    let write_byte buf c = Buffer.add_char buf c in
    let write buf s i len = Buffer.add_subbytes buf s i len in
    let flush _buf = () in
    { close; write_byte; write; flush }
end

module TString = struct
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
end

module TChannel = struct
  let write : out_channel transport_write =
    { close = close_out_noerr; write_byte = output_char; write = output; flush }

  let read : in_channel transport_read =
    { close = close_in_noerr; read = input; read_byte = input_char }

  let with_read_file file f =
    let ic = open_in_bin file in
    let@ () = Fun.protect ~finally:(fun () -> close_in_noerr ic) in
    f @@ TR_read (read, ic)

  let with_write_file file f =
    let oc = open_out_bin file in
    let@ () = Fun.protect ~finally:(fun () -> close_out_noerr oc) in
    f @@ TR_write (write, oc)
end

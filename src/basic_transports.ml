(** Basic transports *)

open Types

module TBuffer = struct
  let write : Buffer.t transport_write =
    let close _buf = () in
    let is_closed _buf = false in
    let write_byte buf c = Buffer.add_char buf c in
    let write buf s i len = Buffer.add_subbytes buf s i len in
    let flush _buf = () in
    { close; is_closed; write_byte; write; flush }
end

module TString = struct
  type reader = { s: string; mutable off: int }

  let create_reader s : reader = { s; off = 0 }

  let read : reader transport_read =
    let is_closed self = self.off >= String.length self.s in
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
    { is_closed; close; read_byte; read }

  let read_any s = TR_read (read, create_reader s)
end

let transport_write_file (file : string) : _ transport_write = assert false

(** Transport that reads from a string *)
let transport_of_string (s : string) : transport_read_any =
  let off = ref 0 in
  let module M = struct
    let is_closed () = !off >= String.length s
    let close () = ()

    let read_byte () =
      let c = s.[!off] in
      incr off;
      c

    let read buf i len =
      let len = min len (String.length s - !off) in
      Bytes.blit_string s !off buf i len;
      off := !off + len;
      len
  end in
  (module M)

let transport_write_file (file : string) : transport_write =
  let oc = open_out_bin file in
  let closed = ref false in
  let module M = struct
    let close () =
      if not !closed then (
        closed := true;
        close_out oc
      )

    let is_closed () = !closed
    let flush () = flush oc

    let write_byte c =
      assert (not !closed);
      output_char oc c

    let write buf i len =
      assert (not !closed);
      output oc buf i len
  end in
  (module M)

(** Transport that reads from a file *)
let transport_read_file (file : string) : transport_read =
  let ic = open_in_bin file in
  let closed = ref false in
  let module M = struct
    let close () =
      if not !closed then (
        closed := true;
        close_in ic
      )

    let is_closed () = !closed
    let read_byte () = input_char ic

    let read buf i len =
      let n = input ic buf i len in
      if n = 0 then close ();
      n
  end in
  (module M)

let with_transport_write_file file (f : transport_write -> 'a) : 'a =
  let ((module W) as tr) = transport_write_file file in
  Fun.protect ~finally:W.close (fun () -> f tr)

let with_transport_read_file file (f : transport_read -> 'a) : 'a =
  let ((module R) as tr) = transport_read_file file in
  Fun.protect ~finally:R.close (fun () -> f tr)

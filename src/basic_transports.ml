(** Basic transports *)

open Types

(** A transport that collects data into a buffer *)
class transport_buffer =
  object
    val buf : Buffer.t = Buffer.create 32
    inherit transport_write

    method contents = Buffer.contents buf
    (** Obtain the content of the buffer *)

    method clear = Buffer.clear buf
    (** Clear buffer *)

    method close = ()
    method is_closed = false
    method write_byte c = Buffer.add_char buf c
    method write s i len = Buffer.add_subbytes buf s i len
    method flush = ()
  end

(** Transport that reads from a string *)
let transport_of_string (s : string) : transport_read =
  let off = ref 0 in
  object
    inherit transport_read
    method is_closed = !off >= String.length s
    method close = ()

    method read_byte =
      let c = s.[!off] in
      incr off;
      c

    method read buf i len =
      let len = min len (String.length s - !off) in
      Bytes.blit_string s !off buf i len;
      off := !off + len;
      len
  end

(** Transport that writes into a file *)
let transport_write_file (file : string) : transport_write =
  let oc = open_out_bin file in
  object
    inherit transport_write
    val mutable closed = false

    method close =
      if not closed then (
        closed <- true;
        close_out oc
      )

    method is_closed = closed
    method flush = flush oc

    method write_byte c =
      assert (not closed);
      output_char oc c

    method write buf i len =
      assert (not closed);
      output oc buf i len
  end

(** Transport that reads from a file *)
let transport_read_file (file : string) : transport_read =
  let ic = open_in_bin file in
  object (self)
    inherit transport_read
    val mutable closed = false

    method close =
      if not closed then (
        closed <- true;
        close_in ic
      )

    method is_closed = closed
    method read_byte = input_char ic

    method read buf i len =
      let n = input ic buf i len in
      if n = 0 then self#close;
      n
  end

let with_transport_write_file file (f : transport_write -> 'a) : 'a =
  let tr = transport_write_file file in
  Fun.protect ~finally:(fun () -> tr#close) (fun () -> f tr)

let with_transport_read_file file (f : transport_read -> 'a) : 'a =
  let tr = transport_read_file file in
  Fun.protect ~finally:(fun () -> tr#close) (fun () -> f tr)

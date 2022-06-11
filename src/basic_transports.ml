(** Basic transports *)

open Types

let transport_of_buffer (buf : Buffer.t) : transport_write =
  let module M = struct
    let close () = ()
    let is_closed () = false
    let write_byte c = Buffer.add_char buf c
    let write s i len = Buffer.add_subbytes buf s i len
    let flush () = ()
  end in
  (module M)

(** Transport that reads from a string *)
let transport_of_string (s : string) : transport_read =
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

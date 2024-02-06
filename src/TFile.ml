open Types
open Common_

let with_read_file file f =
  let ic = open_in_bin file in
  let@ () = Fun.protect ~finally:(fun () -> close_in_noerr ic) in
  f @@ TR_read (TChannel.read, ic)

let with_write_file file f =
  let oc = open_out_bin file in
  let@ () = Fun.protect ~finally:(fun () -> close_out_noerr oc) in
  f @@ TR_write (TChannel.write, oc)

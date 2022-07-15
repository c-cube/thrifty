open Lwt.Syntax
module C = Thrifty_lwt.Client
module Calc_client = Calculator.Client_s

let () =
  let addr = Unix.ADDR_UNIX "./calc" in
  Lwt_main.run
  @@ let* client = C.connect Thrifty.Binary_protocol.protocol addr in

     let* sum = C.call_exn client @@ Calc_client.add ~a:3l ~b:5l in
     let* () = Lwt_io.printlf "%ld + %ld = %ld" 3l 5l sum in

     let* p0 = C.call_exn client Calc_client.get_pings in
     let* () = C.call_oneway client Calc_client.ping
     and* () = C.call_oneway client Calc_client.ping in
     let* p = C.call_exn client Calc_client.get_pings in
     assert (Int32.(p = add p0 2l));
     let* () = C.call_oneway client Calc_client.ping in
     let* p = C.call_exn client Calc_client.get_pings in
     assert (Int32.(p = add p0 3l));
     let* () = Lwt_io.printlf "pings: %ld" p in

     (* now in parallel *)
     let* () = Lwt_io.printlf "open %d parallel queries" 50 in

     let t0 = Unix.gettimeofday () in
     let* l =
       List.init 50 (fun i -> Int32.of_int i)
       |> Lwt_list.map_p (fun i ->
              C.call_exn client @@ Calc_client.add ~a:i ~b:i)
     in
     let elapsed = Unix.gettimeofday () -. t0 in

     let* () =
       Lwt_io.printlf "got results in %.3fs: [%s]" elapsed
         (String.concat ";" @@ List.map Int32.to_string l)
     in

     assert (l = List.init 50 (fun i -> Int32.of_int @@ (i * 2)));

     let* () = Lwt_io.printl "done" in
     Lwt.return ()

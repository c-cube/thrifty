module C = Calculator
open Lwt.Syntax

let debug = ref false
let delay = ref 0.2
let () = Printexc.record_backtrace true

class calculator =
  object
    val mutable n_pings = 0
    inherit Calculator.server_s
    method ping () = n_pings <- 1 + n_pings

    method get_pings () ~reply =
      if !debug then Printf.eprintf "get pings (n=%d)\n%!" n_pings;
      reply (Ok (Int32.of_int n_pings))

    method add ~a ~b () ~reply =
      if !debug then Printf.eprintf "add %ld+%ld\n%!" a b;
      Lwt.async (fun () ->
          let+ () = Lwt_unix.sleep !delay in
          reply (Ok (Int32.add a b)))

    method div ~a ~b () ~reply =
      if !debug then Printf.eprintf "div %ld/%ld\n%!" a b;
      Lwt.async (fun () ->
          let+ () = Lwt_unix.sleep !delay in
          if b = 0l then
            reply (Error Calculator.Div_by_zero)
          else
            reply (Ok (Int32.div a b)))

    method add_all ~l () ~reply : unit =
      if !debug then Printf.eprintf "add all\n%!";
      let s = ref 0l in
      List.iter (fun x -> s := Int32.add x !s) l.ints;
      Lwt.async (fun () ->
          let+ () = Lwt_unix.sleep !delay in
          reply (Ok !s))
  end

let calc = (new calculator :> service_any)

let () =
  let opts =
    [
      "-d", Arg.Set debug, " debug mode";
      "--delay", Arg.Set_float delay, " set delay (in s)";
    ]
    |> Arg.align
  in

  Arg.parse opts (fun _ -> ()) "server.exe";

  let file = "./calc" in
  let addr = Unix.ADDR_UNIX file in
  (try Sys.remove file with _ -> ());
  let stopped, _stop = Lwt.wait () in
  Lwt_main.run
  @@ let* () = Lwt_io.printlf "listening" in
     let* _server =
       Thrifty_lwt.Server.create_server Thrifty.Binary_protocol.protocol calc
         addr
     in
     stopped

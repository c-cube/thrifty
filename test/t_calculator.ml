module Fmt = CCFormat

let () = Printexc.record_backtrace true

class calculator =
  object
    val mutable n_pings = 0
    inherit Calculator.server_s
    method ping () = n_pings <- 1 + n_pings
    method get_pings () ~reply = reply (Ok (Int32.of_int n_pings))
    method add ~a ~b () ~reply = reply (Ok (Int32.add a b))

    method div ~a ~b () ~reply =
      if b = 0l then raise Calculator.Div_by_zero;
      reply (Ok (Int32.div a b))

    method add_all ~l () ~reply =
      let s = ref 0l in
      List.iter (fun x -> s := Int32.add x !s) l.ints;
      reply (Ok !s)
  end

let calc = (new calculator :> service_any)

let () =
  Fmt.printf "%ld + %ld = %ld@." 3l 5l
    (Direct_call.call_with_reply calc @@ Calculator.Client_s.add ~a:3l ~b:5l)

let () =
  let l = CCList.init 100 Int32.of_int in
  Fmt.printf "sum %a = %ld@."
    Fmt.(Dump.list int32)
    l
    (Direct_call.call_with_reply calc
    @@ Calculator.(Client_s.add_all ~l:{ ints = l }))

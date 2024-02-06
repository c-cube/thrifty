(** Multiplex services.

   With this, a single connection can host multiple services,
   with message types written as "servicename:methodname". *)

open Types

(** Multiplex multiple services *)
let service_multiplex (l : service list) : service =
  {
    name = "multiplex";
    process =
      (fun pr pw rd ~reply : unit ->
        let name, typ, seq = pr.read_msg_begin rd in

        let service_name, meth_name =
          match String.index name ':' with
          | exception Not_found ->
            failwith (Printf.sprintf "expected multiplexed name")
          | i ->
            ( String.sub name 0 i,
              String.sub name (i + 1) (String.length name - i - 1) )
        in

        let service : service =
          try List.find (fun s -> s.name = service_name) l
          with Not_found ->
            failwith (Printf.sprintf "unknown service %S" service_name)
        in

        let pr2 = { pr with read_msg_begin = (fun _ -> meth_name, typ, seq) } in
        service.process pr2 pw rd ~reply);
  }

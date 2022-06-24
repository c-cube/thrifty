(** Multiplex services.

   With this, a single connection can host multiple services,
   with message types written as "servicename:methodname". *)

open Types

(** Multiplex multiple services *)
let service_multiplex (l : service_any list) : service_any =
  object
    inherit service_any
    method name = ""

    method process ((module R) : protocol_read) ~reply : unit =
      let name, typ, seq = R.read_msg_begin () in

      let service_name, meth_name =
        match String.index name ':' with
        | exception Not_found ->
          failwith (Printf.sprintf "expected multiplexed name")
        | i ->
          ( String.sub name 0 i,
            String.sub name (i + 1) (String.length name - i - 1) )
      in

      let service =
        try List.find (fun s -> s#name = service_name) l
        with Not_found ->
          failwith (Printf.sprintf "unknown service %S" service_name)
      in

      let module R2 = struct
        include R

        let read_msg_begin () = meth_name, typ, seq
      end in
      service#process (module R2) ~reply
  end

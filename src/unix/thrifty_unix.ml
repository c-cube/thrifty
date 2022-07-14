(** Unix IOs for Thrift *)

(** Read a single frame from the FD.
    @raise End_of_file if a frame could not be read. *)
let read_frame (fd : Unix.file_descr) : string =
  let[@unroll 1] rec really_read b i len =
    if len > 0 then (
      let n = Unix.read fd b i len in
      if n = 0 then raise End_of_file;
      really_read b (i + n) (len - n)
    )
  in

  let b = Bytes.create 4 in
  really_read b 0 4;
  (* size is 4 bytes in big endian (from TFramedTransport.java) *)
  let len = Int32.to_int @@ Bytes.get_int32_be b 0 in
  let b = Bytes.create len in
  really_read b 0 len;
  Bytes.unsafe_to_string b

(** Read a single frame from the FD, and get a transport from it *)
let read_frame_tr (fd : Unix.file_descr) : transport_read =
  let f = read_frame fd in
  Thrifty.Basic_transports.transport_of_string f

(** Write a frame into the file descr *)
let write_frame (fd : Unix.file_descr) (s : string) : unit =
  let[@unroll 1] rec really_write b i len =
    if len > 0 then (
      let n = Unix.write fd b i len in
      really_write b (i + n) (len - n)
    )
  in

  let b = Bytes.create 4 in
  let len = String.length s in
  Bytes.set_int32_be b 0 (Int32.of_int len);
  really_write b 0 4;
  really_write (Bytes.unsafe_of_string s) 0 len

(** A client using the framed transport and the given protocol. *)
module Client : sig
  type t

  val create : protocol -> Unix.file_descr -> t
  val call : t -> 'res client_outgoing_call -> ('res, exn) result
  val call_exn : t -> 'res client_outgoing_call -> 'res
  val call_oneway : t -> client_outgoing_oneway -> unit
end = struct
  type t = {
    fd: Unix.file_descr;
    proto: protocol;
    buf: Buffer.t;
    mutable seq_num: int;
  }

  let create proto fd : t = { fd; proto; buf = Buffer.create 32; seq_num = 0 }

  let call_exn (self : t) c =
    (* Perform a blocking call *)
    let (module Proto) = self.proto in
    let n = self.seq_num in
    self.seq_num <- n + 1;
    Buffer.clear self.buf;

    let (), read_res =
      c ~seq_num:n
        (Proto.write @@ Thrifty.Basic_transports.transport_of_buffer self.buf)
    in

    write_frame self.fd (Buffer.contents self.buf);
    let tr_res = read_frame_tr self.fd in
    let res = read_res (Proto.read tr_res) in
    res

  let call self c = try Ok (call_exn self c) with e -> Error e

  let call_oneway (self : t) (c : client_outgoing_oneway) : unit =
    let (module Proto) = self.proto in
    let n = self.seq_num in
    self.seq_num <- n + 1;
    Buffer.clear self.buf;

    c ~seq_num:n
      (Proto.write @@ Thrifty.Basic_transports.transport_of_buffer self.buf);
    write_frame self.fd (Buffer.contents self.buf);
    ()
end

module Server : sig
  type t

  val create : protocol -> Unix.file_descr -> t

  val serve_loop : t -> service_any -> unit
  (** Read queries from [fd], process them using [service] *)
end = struct
  let ( let@ ) f x = f x

  type t = {
    proto: protocol;
    buf: Buffer.t;
    fd: Unix.file_descr;
    lock: Mutex.t;
  }

  let create proto fd : t =
    { proto; fd; buf = Buffer.create 32; lock = Mutex.create () }

  let[@inline] with_lock self f =
    Mutex.lock self.lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock self.lock) f

  let serve_loop (self : t) (service : service_any) : unit =
    let (module Proto) = self.proto in
    let continue = ref true in

    while !continue do
      match read_frame_tr self.fd with
      | exception End_of_file -> continue := false
      | read_tr ->
        let reply write_answer =
          let@ () = with_lock self in

          Buffer.clear self.buf;
          write_answer
            (Proto.write
            @@ Thrifty.Basic_transports.transport_of_buffer self.buf);
          let resp = Buffer.contents self.buf in
          write_frame self.fd resp
        in

        service#process (Proto.read read_tr) ~reply
    done
end

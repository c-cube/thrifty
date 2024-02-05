(** Unix IOs for Thrift *)

let ( let@ ) = ( @@ )

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
  val with_connect : protocol -> Unix.sockaddr -> (t -> 'a) -> 'a
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

  let with_connect proto sockaddr f =
    let fd =
      match sockaddr with
      | Unix.ADDR_UNIX _ -> Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
      | Unix.ADDR_INET _ ->
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.setsockopt sock Unix.TCP_NODELAY true;
        Unix.setsockopt sock Unix.SO_REUSEADDR true;
        sock
    in
    Unix.connect fd sockaddr;
    let finally () = Unix.close fd in
    let c = create proto fd in
    Fun.protect ~finally (fun () -> f c)

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

  val with_bind :
    ?spawn:((unit -> unit) -> unit) ->
    protocol ->
    Unix.sockaddr ->
    service_any ->
    (t -> 'a) ->
    'a

  val serve : t -> unit
end = struct
  let[@inline] with_lock lock f =
    Mutex.lock lock;
    Fun.protect ~finally:(fun () -> Mutex.unlock lock) f

  let handle_client ~active (proto : protocol) fd (service : service_any) : unit
      =
    let (module Proto) = proto in
    let buf = Buffer.create 32 in
    let lock = Mutex.create () in

    while Atomic.get active do
      match read_frame_tr fd with
      | exception End_of_file -> Atomic.set active false
      | read_tr ->
        let reply write_answer =
          let@ () = with_lock lock in

          Buffer.clear buf;
          write_answer
            (Proto.write @@ Thrifty.Basic_transports.transport_of_buffer buf);
          let resp = Buffer.contents buf in
          write_frame fd resp
        in

        service#process (Proto.read read_tr) ~reply
    done

  type t = {
    fd: Unix.file_descr;
    proto: protocol;
    active: bool Atomic.t;
    service: service_any;
    spawn: (unit -> unit) -> unit;
  }

  let serve (self : t) : unit =
    while Atomic.get self.active do
      let client_fd, _addr = Unix.accept self.fd in
      self.spawn (fun () ->
          handle_client ~active:self.active self.proto client_fd self.service)
    done

  let default_spawn f = ignore (Thread.create f () : Thread.t)

  let with_bind ?(spawn = default_spawn) proto sockaddr service f =
    let fd =
      match sockaddr with
      | Unix.ADDR_UNIX _ -> Unix.socket Unix.PF_UNIX Unix.SOCK_STREAM 0
      | Unix.ADDR_INET _ ->
        let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
        Unix.setsockopt sock Unix.TCP_NODELAY true;
        Unix.setsockopt sock Unix.SO_REUSEADDR true;
        sock
    in
    Unix.bind fd sockaddr;
    Unix.listen fd 16;
    let finally () = Unix.close fd in
    let server : t = { proto; fd; service; spawn; active = Atomic.make true } in
    Fun.protect ~finally (fun () -> f server)
end

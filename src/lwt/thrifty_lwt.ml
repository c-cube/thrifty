open Lwt.Syntax

(** Read a single frame from the FD. *)
let read_frame (ic : Lwt_io.input_channel) : string Lwt.t =
  let b = Bytes.create 4 in
  let* () = Lwt_io.read_into_exactly ic b 0 4 in
  (* size is 4 bytes in big endian (from TFramedTransport.java) *)
  let len = Int32.to_int @@ Bytes.get_int32_be b 0 in
  let b = Bytes.create len in
  let+ () = Lwt_io.read_into_exactly ic b 0 len in
  Bytes.unsafe_to_string b

(** Read a single frame from the FD, and get a transport from it *)
let read_frame_tr (ic : Lwt_io.input_channel) : transport_read Lwt.t =
  let+ f = read_frame ic in
  Thrifty.Basic_transports.transport_of_string f

(** Write a frame into the file descr *)
let write_frame (oc : Lwt_io.output_channel) (s : string) : unit Lwt.t =
  let b = Bytes.create 4 in
  let len = String.length s in
  Bytes.set_int32_be b 0 (Int32.of_int len);
  let* () = Lwt_io.write_from_exactly oc b 0 4 in
  Lwt_io.write_from_exactly oc (Bytes.unsafe_of_string s) 0 len

(** A client using the framed transport and the given protocol. *)
module Client : sig
  type t

  val create : protocol -> Lwt_io.input_channel -> Lwt_io.output_channel -> t
  (** Create a client on the given channels. This client contains state related
      to in-flight queries, and sequence numbers. *)

  val connect : protocol -> Unix.sockaddr -> t Lwt.t

  val stop : t -> unit
  (** Stop the client. *)

  exception Timeout

  val call :
    ?timeout_s:float ->
    t ->
    'res client_outgoing_call ->
    ('res, exn) result Lwt.t

  val call_exn :
    ?timeout_s:float -> t -> 'res client_outgoing_call -> 'res Lwt.t

  val call_oneway : t -> client_outgoing_oneway -> unit Lwt.t
end = struct
  module Int_tbl = Hashtbl.Make (struct
    type t = int

    let equal : t -> t -> bool = ( = )
    let hash i = Hashtbl.hash i
  end)

  exception Timeout

  type t = {
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
    proto: protocol;
    buf: Buffer.t;
    promises: ((string, exn) result -> unit) Int_tbl.t; (* waiting for reply *)
    mutable receive_loop: unit Lwt.t;
    mutable stop: unit Lwt.u;
    mutable seq_num: int;
  }

  let stop self = Lwt.wakeup_later self.stop ()

  let receive self wait_stop : unit Lwt.t =
    let (module Proto) = self.proto in
    let rec loop () =
      let* frame =
        Lwt.catch
          (fun () ->
            let+ f = read_frame self.ic in
            Some f)
          (function
            | End_of_file -> Lwt.return None
            | e -> raise e)
      in
      match frame with
      | None -> Lwt.return () (* done *)
      | Some f ->
        (* find seq num by parsing the message header *)
        let (module P : PROTOCOL_READ) =
          Proto.read @@ Thrifty.Basic_transports.transport_of_string f
        in
        let _, _, n = P.read_msg_begin () in
        (* wakup corresponding promise *)
        (match Int_tbl.find self.promises n with
        | exception Not_found -> ()
        | prom -> prom (Ok f));
        loop ()
    in
    Lwt.pick [ wait_stop; loop () ]

  let create proto ic oc : t =
    let wait_stop, stop = Lwt.wait () in
    let self =
      {
        ic;
        oc;
        proto;
        buf = Buffer.create 32;
        seq_num = 0;
        promises = Int_tbl.create 32;
        stop;
        receive_loop = Lwt.return ();
      }
    in
    self.receive_loop <- receive self wait_stop;
    self

  let connect proto addr : t Lwt.t =
    let dom = Unix.domain_of_sockaddr addr in
    let sock = Lwt_unix.socket dom Unix.SOCK_STREAM 0 in
    let* () = Lwt_unix.connect sock addr in
    let ic = Lwt_io.of_fd ~mode:Lwt_io.input sock in
    let oc = Lwt_io.of_fd ~mode:Lwt_io.output sock in
    Lwt.return @@ create proto ic oc

  let call_exn ?timeout_s (self : t) c : _ Lwt.t =
    (* Perform a blocking call *)
    let (module Proto) = self.proto in
    let n = self.seq_num in
    self.seq_num <- n + 1;
    Buffer.clear self.buf;

    let fut, promise = Lwt.wait () in

    let (), read_res =
      c ~seq_num:n
        (Proto.write @@ Thrifty.Basic_transports.transport_of_buffer self.buf)
    in

    Option.iter
      (fun t ->
        Lwt.async (fun () ->
            let+ () = Lwt_unix.sleep t in
            (* see if the promise is still there *)
            match Int_tbl.find self.promises n with
            | exception Not_found -> ()
            | f -> f (Error Timeout)))
      timeout_s;

    (* register [promise] for when the answer comes *)
    Int_tbl.add self.promises n (fun (res : _ result) ->
        match res with
        | Ok frame ->
          let res =
            read_res
              (Proto.read @@ Thrifty.Basic_transports.transport_of_string frame)
          in
          Lwt.wakeup_later promise res
        | Error e -> Lwt.wakeup_later_exn promise e);

    (* send query *)
    let* () = write_frame self.oc (Buffer.contents self.buf) in
    fut

  let call ?timeout_s self c =
    Lwt.catch
      (fun () ->
        let+ r = call_exn ?timeout_s self c in
        Ok r)
      (fun e -> Lwt.return (Error e))

  let call_oneway (self : t) (c : client_outgoing_oneway) : unit Lwt.t =
    let (module Proto) = self.proto in
    let n = self.seq_num in
    self.seq_num <- n + 1;
    Buffer.clear self.buf;

    c ~seq_num:n
      (Proto.write @@ Thrifty.Basic_transports.transport_of_buffer self.buf);
    write_frame self.oc (Buffer.contents self.buf)
end

module Server : sig
  type t

  val create :
    ?stop:(unit -> unit) ->
    protocol ->
    Lwt_io.input_channel ->
    Lwt_io.output_channel ->
    t

  val stop : t -> unit

  val serve_loop : t -> service_any -> unit Lwt.t
  (** Read queries from [fd], process them using [service] *)

  val create_server :
    protocol -> service_any -> Unix.sockaddr -> Lwt_io.server Lwt.t
  (** Server on the given address, serving the given service *)
end = struct
  type t = {
    proto: protocol;
    buf: Buffer.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
    stop: unit -> unit;
  }

  let create ?(stop = ignore) proto ic oc : t =
    { stop; proto; ic; oc; buf = Buffer.create 32 }

  let stop self = self.stop ()

  let serve_loop (self : t) (service : service_any) : unit Lwt.t =
    let (module Proto) = self.proto in
    let rec loop () : unit Lwt.t =
      let* f =
        Lwt.catch
          (fun () ->
            let+ f = read_frame self.ic in
            Some f)
          (function
            | End_of_file -> Lwt.return None
            | e -> raise e)
      in
      match f with
      | None -> Lwt.return ()
      | Some f ->
        (* to reply: encode, then send response frame asynchronously *)
        let reply write_answer =
          Buffer.clear self.buf;
          write_answer
            (Proto.write
            @@ Thrifty.Basic_transports.transport_of_buffer self.buf);
          let resp = Buffer.contents self.buf in
          Lwt.async (fun () -> write_frame self.oc resp)
        in

        service#process
          (Proto.read @@ Thrifty.Basic_transports.transport_of_string f)
          ~reply;
        loop ()
    in
    loop ()

  let create_server proto (service : service_any) addr : Lwt_io.server Lwt.t =
    Lwt_io.establish_server_with_client_address addr (fun _addr (ic, oc) ->
        let self = create proto ic oc in
        serve_loop self service)
end

(* generated from "foo.thrift" using smol_thrift codegen *)
[@@@ocaml.warning {|-26-27-39|}]
let pp_pair ppk ppv out (k,v) = Format.fprintf out {|(%a,%a)|} ppk k ppv v
let pp_list ppx out l = Format.fprintf out {|[@[%a@]]|}
   (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out {|;@ |}) ppx) l
open Smol_thrift.Types

let n : int32 = 256l

type foo = {
  x: int32 option;
  y: string option;
  z: bool
}

let rec pp_foo out (self:foo) =
  Format.fprintf out "{@[";
  (match self.x with
   | None -> ()
   | Some x -> Format.fprintf out {|x=%a|}
     (fun out self -> Format.fprintf out "%ld" self) x);
  (match self.y with
   | None -> ()
   | Some x -> Format.fprintf out {|y=%a|}
     (fun out self -> Format.fprintf out "%S" self) x);
  Format.fprintf out "z=%a"
    (fun out self -> Format.fprintf out "%B" self) self.z;
  Format.fprintf out "@]}"

(** Serialize *)
let rec write_foo (module OP:PROTOCOL_WRITE) (self:foo) : unit =
  let {x; y; z} = self in
  OP.write_struct_begin "foo";
  begin
    (match y with
     | None -> ()
     | Some x ->
       OP.write_field_begin "y" T_STRING 1;
       OP.write_string x;
       OP.write_field_end());
    (match x with
     | None -> ()
     | Some x ->
       OP.write_field_begin "x" T_I32 2;
       OP.write_i32 x;
       OP.write_field_end());
    begin
      OP.write_field_begin "z" T_BOOL 5;
      OP.write_bool z;
      OP.write_field_end();
    end
  end;
  OP.write_struct_end ()

(** Deserialize *)
let rec read_foo (module IP:PROTOCOL_READ) : foo =
  let _name = IP.read_struct_begin () in
  let continue = ref true in
  let x = ref (Some (1l)) in
  let y = ref None in
  let z = ref None in
  while !continue do
    match IP.read_field_begin () with
    | exception Smol_thrift.Types.Read_stop_field -> continue := false
    | ("x", T_I32, _) | (_, T_I32, 2) ->
      x := Some(IP.read_i32 ());
    | ("x", _, _) | (_, _, 2) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (2: "x")|}))
    | ("y", (T_STRING | T_BINARY), _) | (_, (T_STRING | T_BINARY), 1) ->
      y := Some(IP.read_string ());
    | ("y", _, _) | (_, _, 1) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (1: "y")|}))
    | ("z", T_BOOL, _) | (_, T_BOOL, 5) ->
      z := Some(IP.read_bool ());
    | ("z", _, _) | (_, _, 5) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (5: "z")|}))
    | _ -> () (* unknown field *)
  done;
  IP.read_struct_end ();
  let x = !x in
  let y = !y in
  let z = match !z with
    | None ->
      raise (Runtime_error (UE_invalid_protocol,
               {|field (5: "z") is required|}))
    | Some x -> x in
  {x;y;z}

type loc = {
  long: float;
  lat: float
}

let rec pp_loc out (self:loc) =
  Format.fprintf out "{@[";
  Format.fprintf out "long=%a"
    (fun out self -> Format.fprintf out "%f" self) self.long;
  Format.fprintf out "lat=%a"
    (fun out self -> Format.fprintf out "%f" self) self.lat;
  Format.fprintf out "@]}"

(** Serialize *)
let rec write_loc (module OP:PROTOCOL_WRITE) (self:loc) : unit =
  let {long; lat} = self in
  OP.write_struct_begin "loc";
  begin
    begin
      OP.write_field_begin "long" T_DOUBLE 1;
      OP.write_double long;
      OP.write_field_end();
    end;
    begin
      OP.write_field_begin "lat" T_DOUBLE 2;
      OP.write_double lat;
      OP.write_field_end();
    end
  end;
  OP.write_struct_end ()

(** Deserialize *)
let rec read_loc (module IP:PROTOCOL_READ) : loc =
  let _name = IP.read_struct_begin () in
  let continue = ref true in
  let long = ref None in
  let lat = ref None in
  while !continue do
    match IP.read_field_begin () with
    | exception Smol_thrift.Types.Read_stop_field -> continue := false
    | ("long", T_DOUBLE, _) | (_, T_DOUBLE, 1) ->
      long := Some(IP.read_double ());
    | ("long", _, _) | (_, _, 1) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (1: "long")|}))
    | ("lat", T_DOUBLE, _) | (_, T_DOUBLE, 2) ->
      lat := Some(IP.read_double ());
    | ("lat", _, _) | (_, _, 2) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (2: "lat")|}))
    | _ -> () (* unknown field *)
  done;
  IP.read_struct_end ();
  let long = match !long with
    | None ->
      raise (Runtime_error (UE_invalid_protocol,
               {|field (1: "long") is required|}))
    | Some x -> x in
  let lat = match !lat with
    | None ->
      raise (Runtime_error (UE_invalid_protocol,
               {|field (2: "lat") is required|}))
    | Some x -> x in
  {long;lat}

exception Ohno

exception Ohno2 of {
  really_bad: bool option
}

exception Ohno3 of {
  why: string option;
  where: loc option
}

type fooK =
  | K1
  | K2
  | K3

let pp_fooK out self =
  let s = match self with | K1 -> "K1" | K2 -> "K2" | K3 -> "K3" in
  Format.fprintf out {|%s|} s

let int_of_fooK = function  | K1 -> 1 | K2 -> 4 | K3 -> 5

let fooK_of_int = function
  | 1 -> K1
  | 4 -> K2
  | 5 -> K3
  | n -> raise (Runtime_error (UE_invalid_protocol, Printf.sprintf "unknown enum member %d for `fooK`" n))

(** Serialize a "fooK" *)
let write_fooK (module OP:PROTOCOL_WRITE) (self:fooK) =
  OP.write_i16 (int_of_fooK self)

(** Deserialize a "fooK" *)
let read_fooK (module IP:PROTOCOL_READ) : fooK =
  IP.read_i16 () |> fooK_of_int

type bar = {
  foos: ((foo) list) list option;
  kind: fooK option
}

let rec pp_bar out (self:bar) =
  Format.fprintf out "{@[";
  (match self.foos with
   | None -> ()
   | Some x -> Format.fprintf out {|foos=%a|} (pp_list (pp_list pp_foo)) x);
  (match self.kind with
   | None -> ()
   | Some x -> Format.fprintf out {|kind=%a|} pp_fooK x);
  Format.fprintf out "@]}"

(** Serialize *)
let rec write_bar (module OP:PROTOCOL_WRITE) (self:bar) : unit =
  let {foos; kind} = self in
  OP.write_struct_begin "bar";
  begin
    (match foos with
     | None -> ()
     | Some x ->
       OP.write_field_begin "foos" T_LIST 1;
       OP.write_list_begin T_LIST (List.length x);
       List.iter
         (fun x ->
          OP.write_list_begin T_STRUCT (List.length x);
          List.iter (fun x -> write_foo (module OP) x) x;
          OP.write_list_end())
         x;
       OP.write_list_end();
       OP.write_field_end());
    (match kind with
     | None -> ()
     | Some x ->
       OP.write_field_begin "kind" T_STRUCT 2;
       write_fooK (module OP) x;
       OP.write_field_end())
  end;
  OP.write_struct_end ()

(** Deserialize *)
let rec read_bar (module IP:PROTOCOL_READ) : bar =
  let _name = IP.read_struct_begin () in
  let continue = ref true in
  let foos = ref (Some ([])) in
  let kind = ref None in
  while !continue do
    match IP.read_field_begin () with
    | exception Smol_thrift.Types.Read_stop_field -> continue := false
    | ("foos", T_LIST, _) | (_, T_LIST, 1) ->
      foos :=
        Some((let _ty, len = IP.read_list_begin () in
              assert (match _ty with T_LIST -> true | _ -> false);
              let l = List.init len
                (fun _i ->
                 (let _ty, len = IP.read_list_begin () in
                  assert (match _ty with T_STRUCT -> true | _ -> false);
                  let l = List.init len (fun _i -> read_foo (module IP) ) in
                  IP.read_list_end(); l)) in
              IP.read_list_end(); l));
    | ("foos", _, _) | (_, _, 1) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (1: "foos")|}))
    | ("kind", T_STRUCT, _) | (_, T_STRUCT, 2) ->
      kind := Some(read_fooK (module IP) );
    | ("kind", _, _) | (_, _, 2) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (2: "kind")|}))
    | _ -> () (* unknown field *)
  done;
  IP.read_struct_end ();
  let foos = !foos in
  let kind = !kind in
  {foos;kind}

type fooOrBarOrBool =
  | Foo of foo
  | Bar of bar
  | B of bool

let rec pp_fooOrBarOrBool out (self:fooOrBarOrBool) =
  match self with
  | Foo self -> Format.fprintf out "Foo (%a)" pp_foo self
  | Bar self -> Format.fprintf out "Bar (%a)" pp_bar self
  | B self -> Format.fprintf out "B (%a)"
    (fun out self -> Format.fprintf out "%B" self) self

(** Serialize *)
let rec write_fooOrBarOrBool (module OP:PROTOCOL_WRITE) (self:fooOrBarOrBool) : unit =
  OP.write_struct_begin "fooOrBarOrBool";
  (match self with
   | Foo x ->
     begin
       OP.write_field_begin "foo" T_STRUCT 1;
       write_foo (module OP) x;
       OP.write_field_end();
     end
   | Bar x ->
     begin
       OP.write_field_begin "bar" T_STRUCT 2;
       write_bar (module OP) x;
       OP.write_field_end();
     end
   | B x ->
     begin
       OP.write_field_begin "b" T_BOOL 3;
       OP.write_bool x;
       OP.write_field_end();
     end);OP.write_struct_end ()

(** Deserialize *)
let rec read_fooOrBarOrBool (module IP:PROTOCOL_READ) : fooOrBarOrBool =
  let _name = IP.read_struct_begin () in
  let continue = ref true in
  let foo = ref None in
  let bar = ref None in
  let b = ref None in
  while !continue do
    match IP.read_field_begin () with
    | exception Smol_thrift.Types.Read_stop_field -> continue := false
    | ("foo", T_STRUCT, _) | (_, T_STRUCT, 1) ->
      foo := Some(read_foo (module IP) );
    | ("foo", _, _) | (_, _, 1) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (1: "foo")|}))
    | ("bar", T_STRUCT, _) | (_, T_STRUCT, 2) ->
      bar := Some(read_bar (module IP) );
    | ("bar", _, _) | (_, _, 2) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (2: "bar")|}))
    | ("b", T_BOOL, _) | (_, T_BOOL, 3) ->
      b := Some(IP.read_bool ());
    | ("b", _, _) | (_, _, 3) ->
      raise (Runtime_error
        (UE_invalid_protocol, {|invalid type for field (3: "b")|}))
    | _ -> () (* unknown field *)
  done;
  IP.read_struct_end ();
  (* check which field is set *)
  (match !foo, !bar, !b with
   | Some x,_,_ -> Foo x
   | _,Some x,_ -> Bar x
   | _,_,Some x -> B x
   | _ -> raise (Runtime_error (UE_protocol_error, Printf.sprintf {|no field set for "fooOrBarOrBool"|}))
   )

type bar2 = (bar) list

let write_bar2 (module OP:PROTOCOL_WRITE) (self:bar2) : unit =
  OP.write_list_begin T_STRUCT (List.length self);
  List.iter (fun x -> write_bar (module OP) x) self;
  OP.write_list_end()

let read_bar2 (module IP:PROTOCOL_READ) : bar2 =
  (let _ty, len = IP.read_list_begin () in
   assert (match _ty with T_STRUCT -> true | _ -> false);
   let l = List.init len (fun _i -> read_bar (module IP) ) in
   IP.read_list_end(); l)

let pp_bar2 out (self:bar2) = pp_list pp_bar out self

(** Server-side for service "giveKind" *)
class virtual server_giveKind = object (self)
  inherit service_any
  method name = "giveKind"

  method virtual get_kind : ?foo:foo -> unit -> fooK

  method virtual send_bar : ?bar:bar -> unit -> unit

  method virtual send_whatev : how_many:int32-> ?k:fooK -> unit -> unit

  (** Process an incoming message *)
  method process (ip:protocol_read) (op:protocol_write) : unit =
    let (module IP) = ip in
    let (module OP) = op in
    let msg_name, msg_ty, seq_num = IP.read_msg_begin () in
    IP.read_msg_end();
    (* reply using a runtime failure *)
    let reply_exn_ (ue:unexpected_exception) (msg:string) : unit =
      OP.write_msg_begin {||} MSG_EXCEPTION seq_num;
      OP.write_msg_end ();
      let ty = Smol_thrift.Types.int_of_unexpected_exception ue in
      begin
        begin
          OP.write_field_begin "type" T_I32 0;
          OP.write_i32 ty;
          OP.write_field_end();
        end;
        begin
          OP.write_field_begin "message" T_STRING 1;
          OP.write_string msg;
          OP.write_field_end();
        end
      end
    in
    try (
    match msg_name, msg_ty with
    | "get_kind", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let foo = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Smol_thrift.Types.Read_stop_field -> continue := false
        | ("foo", T_STRUCT, _) | (_, T_STRUCT, 1) ->
          foo := Some(read_foo (module IP) );
        | ("foo", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "foo")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let foo = !foo in
      (* call the user code *)
      (match self#get_kind ?foo () with
       | res ->
         OP.write_msg_begin {||} MSG_REPLY seq_num;
         OP.write_msg_end();
         OP.write_struct_begin {||};
         begin
           OP.write_field_begin "success" T_STRUCT 0;
           write_fooK (module OP) res;
           OP.write_field_end();
         end;
         OP.write_field_stop();
         OP.write_struct_end ()
       | exception exn ->
         raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       )
    | "send_bar", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let bar = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Smol_thrift.Types.Read_stop_field -> continue := false
        | ("bar", T_STRUCT, _) | (_, T_STRUCT, 1) ->
          bar := Some(read_bar (module IP) );
        | ("bar", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "bar")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let bar = !bar in
      (* call the user code *)
      (match self#send_bar ?bar () with
       | res ->
         OP.write_msg_begin {||} MSG_REPLY seq_num;
         OP.write_msg_end();
         OP.write_struct_begin {||};
         OP.write_field_stop();
         OP.write_struct_end ()
       | exception Ohno ->
         OP.write_msg_begin {||} MSG_REPLY seq_num;
         OP.write_msg_end();
         OP.write_struct_begin {||};
         OP.write_field_begin {|o|} T_STRUCT 2;
         OP.write_field_stop();
         OP.write_field_end();
         OP.write_field_stop();
         OP.write_struct_end ()
       | exception (Ohno2 {really_bad}) ->
         OP.write_msg_begin {||} MSG_REPLY seq_num;
         OP.write_msg_end();
         OP.write_struct_begin {||};
         OP.write_field_begin {|o2|} T_STRUCT 3;
         begin
           (match really_bad with
            | None -> ()
            | Some x ->
              OP.write_field_begin "really_bad" T_BOOL 1;
              OP.write_bool x;
              OP.write_field_end())
         end;
         OP.write_field_stop();
         OP.write_field_end();
         OP.write_field_stop();
         OP.write_struct_end ()
       | exception exn ->
         raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       )
    | "send_whatev", MSG_ONEWAY ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let how_many = ref None in
      let k = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Smol_thrift.Types.Read_stop_field -> continue := false
        | ("how_many", T_I32, _) | (_, T_I32, 1) ->
          how_many := Some(IP.read_i32 ());
        | ("how_many", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "how_many")|}))
        | ("k", T_STRUCT, _) | (_, T_STRUCT, 3) ->
          k := Some(read_fooK (module IP) );
        | ("k", _, _) | (_, _, 3) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (3: "k")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let how_many = match !how_many with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (1: "how_many") is required|}))
        | Some x -> x in
      let k = !k in
      (try self#send_whatev ~how_many ?k () with _ -> ());
    | _n, _ -> raise (Runtime_error (UE_invalid_message_type, {|invalid message|}));
    ) with Runtime_error (ue, msg) ->
      (* catch runtime errors and reify them *)
      reply_exn_ ue msg;
end

(** Server-side for service "calculator" *)
class virtual server_calculator = object (self)
  inherit service_any
  method name = "calculator"

  method virtual add : x:int32-> y:int32 -> unit -> int32

  method virtual mult : x:int32-> y:int32 -> unit -> int32

  (** Process an incoming message *)
  method process (ip:protocol_read) (op:protocol_write) : unit =
    let (module IP) = ip in
    let (module OP) = op in
    let msg_name, msg_ty, seq_num = IP.read_msg_begin () in
    IP.read_msg_end();
    (* reply using a runtime failure *)
    let reply_exn_ (ue:unexpected_exception) (msg:string) : unit =
      OP.write_msg_begin {||} MSG_EXCEPTION seq_num;
      OP.write_msg_end ();
      let ty = Smol_thrift.Types.int_of_unexpected_exception ue in
      begin
        begin
          OP.write_field_begin "type" T_I32 0;
          OP.write_i32 ty;
          OP.write_field_end();
        end;
        begin
          OP.write_field_begin "message" T_STRING 1;
          OP.write_string msg;
          OP.write_field_end();
        end
      end
    in
    try (
    match msg_name, msg_ty with
    | "add", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let x = ref None in
      let y = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Smol_thrift.Types.Read_stop_field -> continue := false
        | ("x", T_I32, _) | (_, T_I32, 1) ->
          x := Some(IP.read_i32 ());
        | ("x", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "x")|}))
        | ("y", T_I32, _) | (_, T_I32, 2) ->
          y := Some(IP.read_i32 ());
        | ("y", _, _) | (_, _, 2) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (2: "y")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let x = match !x with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (1: "x") is required|}))
        | Some x -> x in
      let y = match !y with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (2: "y") is required|}))
        | Some x -> x in
      (* call the user code *)
      (match self#add ~x ~y () with
       | res ->
         OP.write_msg_begin {||} MSG_REPLY seq_num;
         OP.write_msg_end();
         OP.write_struct_begin {||};
         begin
           OP.write_field_begin "success" T_I32 0;
           OP.write_i32 res;
           OP.write_field_end();
         end;
         OP.write_field_stop();
         OP.write_struct_end ()
       | exception exn ->
         raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       )
    | "mult", MSG_CALL ->
      (* read arguments *)
      let _name = IP.read_struct_begin () in
      let continue = ref true in
      let x = ref None in
      let y = ref None in
      while !continue do
        match IP.read_field_begin () with
        | exception Smol_thrift.Types.Read_stop_field -> continue := false
        | ("x", T_I32, _) | (_, T_I32, 1) ->
          x := Some(IP.read_i32 ());
        | ("x", _, _) | (_, _, 1) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (1: "x")|}))
        | ("y", T_I32, _) | (_, T_I32, 2) ->
          y := Some(IP.read_i32 ());
        | ("y", _, _) | (_, _, 2) ->
          raise (Runtime_error
            (UE_invalid_protocol, {|invalid type for field (2: "y")|}))
        | _ -> () (* unknown field *)
      done;
      IP.read_struct_end();
      let x = match !x with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (1: "x") is required|}))
        | Some x -> x in
      let y = match !y with
        | None ->
          raise (Runtime_error (UE_invalid_protocol,
                   {|field (2: "y") is required|}))
        | Some x -> x in
      (* call the user code *)
      (match self#mult ~x ~y () with
       | res ->
         OP.write_msg_begin {||} MSG_REPLY seq_num;
         OP.write_msg_end();
         OP.write_struct_begin {||};
         begin
           OP.write_field_begin "success" T_I32 0;
           OP.write_i32 res;
           OP.write_field_end();
         end;
         OP.write_field_stop();
         OP.write_struct_end ()
       | exception exn ->
         raise (Runtime_error (UE_internal_error, (Printexc.to_string exn)))
       )
    | _n, _ -> raise (Runtime_error (UE_invalid_message_type, {|invalid message|}));
    ) with Runtime_error (ue, msg) ->
      (* catch runtime errors and reify them *)
      reply_exn_ ue msg;
end


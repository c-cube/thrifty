(* generated from "foo.thrift" using smol_thrift codegen *)
[@@@ocaml.warning {|-26-27|}]
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
let rec write_foo (module OP:PROTOCOL_WRITE) (self:foo) =
  let {x; y; z} = self in
  OP.write_struct_begin "foo";
  begin
    (match x with
     | None -> ()
     | Some x ->
       OP.write_field_begin "x" T_I32 1;
       OP.write_i32 x;
       OP.write_field_end());
    (match y with
     | None -> ()
     | Some x ->
       OP.write_field_begin "y" T_STRING 2;
       OP.write_string x;
       OP.write_field_end());
    begin
      OP.write_field_begin "z" T_BOOL 3;
      OP.write_bool self.z;
      OP.write_field_end();
    end
  end;
  OP.write_struct_end ()

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
let rec write_loc (module OP:PROTOCOL_WRITE) (self:loc) =
  let {long; lat} = self in
  OP.write_struct_begin "loc";
  begin
    begin
      OP.write_field_begin "long" T_DOUBLE 1;
      OP.write_double self.long;
      OP.write_field_end();
    end;
    begin
      OP.write_field_begin "lat" T_DOUBLE 2;
      OP.write_double self.lat;
      OP.write_field_end();
    end
  end;
  OP.write_struct_end ()

exception Ohno

exception Ohno2 of {
  really_bad: bool
}

exception Ohno3 of {
  why: string;
  where: loc
}

type fooK =
  | K1
  | K2
  | K3

let pp_fooK out self = match self with
  | K1 -> Format.fprintf out "K1"
  | K2 -> Format.fprintf out "K2"
  | K3 -> Format.fprintf out "K3"

let int_of_fooK = function  | K1 -> 1 | K2 -> 4 | K3 -> 5

let fooK_of_int = function
  | 1 -> K1
  | 4 -> K2
  | 5 -> K3
  | n -> failwith (Printf.sprintf "unknown enum member %d for `fooK`" n)

(** Serialize a "fooK" *)
let write_fooK (module OP:PROTOCOL_WRITE) (self:fooK) =
  OP.write_i16 (int_of_fooK self)

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
let rec write_bar (module OP:PROTOCOL_WRITE) (self:bar) =
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
let rec write_fooOrBarOrBool (module OP:PROTOCOL_WRITE) (self:fooOrBarOrBool) =
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

type bar2 = (bar) list

let write_bar2 (module OP:PROTOCOL_WRITE) (self:bar2) =
  OP.write_list_begin T_STRUCT (List.length self);
  List.iter (fun x -> write_bar (module OP) x) self;
  OP.write_list_end()

let pp_bar2 out (self:bar2) = pp_list pp_bar out self

(** Server-side for service "giveKind" *)
class virtual server_giveKind = object (self)
  inherit service_any
  method name = "giveKind"

  method virtual get_kind : ?foo:foo -> unit -> fooK

  method virtual send_bar : ?bar:bar -> unit -> unit

  method virtual send_whatev : ?k:fooK -> unit -> unit

  (** Process an incoming message *)
  method process (ip:protocol_read) (op:protocol_write) : unit =
    let (module IP) = ip in
    let msg_name, msg_ty, seq_num = IP.read_msg_begin () in
    IP.read_msg_end();
    match msg_name, msg_ty with
    | "get_kind", MSG_CALL -> assert false (* TODO *)
    | "send_bar", MSG_CALL -> assert false (* TODO *)
    | "send_whatev", MSG_ONEWAY -> assert false (* TODO *)
    | _n, _ -> failwith (Printf.sprintf {|invalid message %S|} _n)

end


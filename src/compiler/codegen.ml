module A = Ast

let fpf = Format.fprintf
let spf = Printf.sprintf
let debug = ref false

(* print with "and" as separator *)
let pp_l_and ppx out l =
  List.iteri
    (fun i x ->
      if i > 0 then fpf out "@]@ @[<v2>and ";
      ppx ~first:(i = 0) out x)
    l

(* print with sep as separator *)
let pp_l ~sep ppx out l =
  List.iteri
    (fun i x ->
      if i > 0 then fpf out "%s@ " sep;
      ppx out x)
    l

module CG : sig
  type t

  val create : unit -> t
  val add_prelude : filename:string -> t -> unit
  val encode_file : t -> pp:bool -> Ast.File.t -> unit
  val code : t -> string
  val write_code : out_channel -> t -> unit
end = struct
  type fmt = Format.formatter
  type t = { buf: Buffer.t; out: fmt }

  let fpf (self : fmt) fmt = Format.fprintf self fmt
  let addstr self = Format.pp_print_string self

  let create () : t =
    let buf = Buffer.create 1024 in
    let out = Format.formatter_of_buffer buf in
    { out; buf }

  let prelude file =
    spf
      "(* generated from %S using smol_thrift codegen *)\n\
       [@@@ocaml.warning {|-26-27|}]\n\
       let pp_pair ppk ppv out (k,v) = Format.fprintf out {|(%%a,%%a)|} ppk k \
       ppv v\n\
       let pp_list ppx out l = Format.fprintf out {|[@[%%a@]]|}\n\
      \   (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out {|;@ \
       |}) ppx) l"
      file

  let add_prelude ~filename self =
    fpf self.out "%s@." (prelude filename);
    fpf self.out "open Smol_thrift.Types@.";
    ()

  let code self =
    fpf self.out "@.";
    Buffer.contents self.buf

  let write_code oc self =
    fpf self.out "@.";
    Buffer.output_buffer oc self.buf

  let mangle_name (s : string) : string = String.uncapitalize_ascii s
  let mangle_cstor (s : string) : string = String.capitalize_ascii s

  let rec pp_ty out (ty : A.Type.t) : unit =
    match ty.view with
    | Named s -> fpf out "%s" (mangle_name s)
    | List ty -> fpf out "(%a) list" pp_ty ty
    | Set ty -> fpf out "(%a) list" pp_ty ty
    | Map (ty1, ty2) -> fpf out "(@[%a * %a@]) list" pp_ty ty1 pp_ty ty2
    | Base b ->
      let s =
        match b with
        | T_BOOL -> "bool"
        | T_BYTE -> "char"
        | T_I8 -> "char"
        | T_I16 -> "int"
        | T_I32 -> "int32"
        | T_I64 -> "int64"
        | T_DOUBLE -> "float"
        | T_STRING -> "string"
        | T_BINARY -> "string"
        | T_STRUCT | T_MAP | T_SET | T_LIST ->
          failwith
          @@ Format.asprintf "bad base type %s" (string_of_field_type b)
      in
      fpf out "%s" s

  let pp_fun_ty out (ty : A.Function_type.t) =
    match ty with
    | Ast.Function_type.Void -> fpf out "unit"
    | Ast.Function_type.Ty ty -> pp_ty out ty

  let rec pp_const_value ~ty out (v : A.Const_value.t) : unit =
    let recurse = pp_const_value ~ty:None in
    match v with
    | Ast.Const_value.Bool b -> fpf out "%b" b
    | Ast.Const_value.Int i ->
      let suff =
        match ty with
        | Some A.Type.{ view = Base T_I32; _ } -> "l"
        | Some A.Type.{ view = Base T_I64; _ } -> "L"
        | _ -> ""
      in
      fpf out "%Ld%s" i suff
    | Ast.Const_value.Double f -> fpf out "%f" f
    | Ast.Const_value.String s -> fpf out "%S" s
    | Ast.Const_value.List l ->
      fpf out "[@[";
      List.iter (fun x -> fpf out "%a;@ " recurse x) l;
      fpf out "@]]"
    | Ast.Const_value.Map l ->
      fpf out "[@[";
      List.iter (fun (x, y) -> fpf out "(@[%a, %a@]);@ " recurse x recurse y) l;
      fpf out "@]]"
    | Ast.Const_value.Named s -> fpf out "%s" s

  (* generate the constant definition *)
  let cg_const (self : t) name ty value : unit =
    let name = mangle_name name in
    fpf self.out "@.@[<2>let %s : %a =@ %a@]@." name pp_ty ty
      (pp_const_value ~ty:(Some ty))
      value

  (** Generate a printer {b expression} for [out (self:ty)] *)
  let rec cg_printer_for_ty out (ty : A.Type.t) : unit =
    match ty.view with
    | Named s -> fpf out "pp_%s out self" (mangle_name s)
    | List ty | Set ty ->
      fpf out "pp_list@ %a@ out self" cg_printer_fun_for_ty ty
    | Map (ty1, ty2) ->
      fpf out "pp_list@ (@[pp_pair@ %a@ %a@])@ out self" cg_printer_fun_for_ty
        ty1 cg_printer_fun_for_ty ty2
    | Base b ->
      let fmt =
        match b with
        | T_BOOL -> "%B"
        | T_BYTE -> "%C"
        | T_I8 -> "%C"
        | T_I16 -> "%d"
        | T_I32 -> "%ld"
        | T_I64 -> "%Ld"
        | T_DOUBLE -> "%f"
        | T_STRING -> "%S"
        | T_BINARY -> "%S"
        | T_STRUCT | T_MAP | T_SET | T_LIST ->
          failwith
          @@ Format.asprintf "bad base type %s" (string_of_field_type b)
      in
      fpf out "Format.fprintf out %S self" fmt

  (** Generate a printer {b function} for this type *)
  and cg_printer_fun_for_ty out (ty : A.Type.t) =
    match ty.view with
    | Named s -> fpf out "pp_%s" (mangle_name s)
    | List ty | Set ty -> fpf out "(@[pp_list@ %a@])" cg_printer_fun_for_ty ty
    | Map (ty1, ty2) ->
      fpf out "(@[pp_list@ (@[pp_pair@ %a@ %a@])@])" cg_printer_fun_for_ty ty1
        cg_printer_fun_for_ty ty2
    | _ -> fpf out "(@[fun out self ->@ %a@])" cg_printer_for_ty ty

  let cg_write_field_ty_of_ty (ty : A.Type.t) =
    match ty.view with
    | A.Type.Named _ -> "T_STRUCT"
    | A.Type.List _ -> "T_LIST"
    | A.Type.Set _ -> "T_SET"
    | A.Type.Map _ -> "T_MAP"
    | A.Type.Base b ->
      (match b with
      | T_BOOL -> "T_BOOL"
      | T_BYTE | T_I8 -> "T_BYTE"
      | T_I16 -> "T_I16"
      | T_I32 -> "T_I32"
      | T_I64 -> "T_I64"
      | T_DOUBLE -> "T_DOUBLE"
      | T_STRING -> "T_STRING"
      | T_BINARY -> "T_BINARY"
      | T_STRUCT -> "T_STRUCT"
      | T_MAP -> "T_MAP"
      | T_SET -> "T_SET"
      | T_LIST -> "T_LIST")

  (** Generate a serializer for [(module OP) (name:ty)] *)
  let rec cg_write_for_ty out ((name, ty) : string * A.Type.t) : unit =
    match ty.view with
    | Named s -> fpf out "write_%s (module OP) %s" (mangle_name s) name
    | List ty ->
      let fty = cg_write_field_ty_of_ty ty in
      fpf out "OP.write_list_begin %s (List.length %s);@ " fty name;
      fpf out "@[<2>List.iter@ (@[<hv>fun x ->@ %a@])@ %s@];@ " cg_write_for_ty
        ("x", ty) name;
      fpf out "OP.write_list_end()"
    | Set ty ->
      let fty = cg_write_field_ty_of_ty ty in
      fpf out "OP.write_set_begin %s (List.length %s);@ " name fty;
      fpf out "@[<2>List.iter@ (@[<hv>fun self ->@ %a@])@ %s@];@ "
        cg_write_for_ty ("x", ty) name;
      fpf out "OP.write_set_end()"
    | Map (ty1, ty2) ->
      let fty1 = cg_write_field_ty_of_ty ty1 in
      let fty2 = cg_write_field_ty_of_ty ty2 in
      fpf out "OP.write_map_begin %s %s (List.length %s);@ " fty1 fty2 name;
      fpf out "@[<2>List.iter@ (@[<hv>fun (k,v) ->@ %a;@ %a@])@ %s@];@ "
        cg_write_for_ty ("k", ty1) cg_write_for_ty ("v", ty2) name;
      fpf out "OP.write_map_end()"
    | Base b ->
      let m =
        match b with
        | T_BOOL -> "write_bool"
        | T_BYTE -> "write_byte"
        | T_I8 -> "write_i8"
        | T_I16 -> "write_i16"
        | T_I32 -> "write_i32"
        | T_I64 -> "write_i64"
        | T_DOUBLE -> "write_double"
        | T_STRING -> "write_string"
        | T_BINARY -> "write_binary"
        | T_STRUCT | T_MAP | T_SET | T_LIST ->
          failwith
          @@ Format.asprintf "bad base type %s" (string_of_field_type b)
      in
      fpf out "OP.%s %s" m name

  let cg_write_field out ((name, field_id, f) : string * int * A.Field.t) : unit
      =
    let fty = cg_write_field_ty_of_ty f.ty in
    if A.Field.is_required f then
      fpf out
        "@[<v2>begin@ OP.write_field_begin %S %s %d;@ %a;@ \
         OP.write_field_end();@;\
         <1 -2>end@]" f.name fty field_id cg_write_for_ty (name, f.ty)
    else
      fpf out
        "(@[<v>match %s with@ | None -> ()@ | @[<v>Some x ->@ \
         OP.write_field_begin %S %s %d;@ %a;@ OP.write_field_end()@]@])"
        (mangle_name f.name) f.name fty field_id cg_write_for_ty ("x", f.ty)

  (* generate code to write fields into a [protocol_write].
     Each field is assumed to be in a variable of the same name,
     the protocol in module [OP] (output protocol) *)
  let cg_write_fields out (fs : (int * A.Field.t) list) : unit =
    let write_field i ((field_id, f) : int * A.Field.t) =
      if i > 0 then fpf out ";@ ";
      let name = spf "self.%s" (mangle_name f.name) in
      cg_write_field out (name, field_id, f)
    in
    fpf out "@[<v2>begin@ ";
    List.iteri write_field fs;
    fpf out "@;<1 -2>end@]"

  let cg_typedef ~pp (self : t) name ty : unit =
    let name = mangle_name name in
    fpf self.out {|@.@[<2>type %s = %a@]@.|} name pp_ty ty;
    fpf self.out
      {|@.@[<v2>let write_%s (module OP:PROTOCOL_WRITE) (self:%s) =@ %a@]@.|}
      name name cg_write_for_ty ("self", ty);
    if pp then
      fpf self.out {|@.@[<2>let pp_%s out (self:%s) =@ %a@]@.|} name name
        cg_printer_for_ty ty

  let cg_enum ~pp (self : t) name cases : unit =
    let name = mangle_name name in

    let cases =
      let n = ref 0 in
      List.map
        (fun { A.Definition.e_name; e_num } ->
          ( e_name,
            match e_num with
            | None ->
              let x = !n in
              incr n;
              x
            | Some i ->
              n := i + 1;
              i ))
        cases
    in

    (* type def *)
    fpf self.out {|@.@[<v2>type %s =|} name;
    List.iter (fun (c, _) -> fpf self.out "@ | %s" (mangle_cstor c)) cases;
    fpf self.out {|@]@.|};

    (* pp *)
    if pp then (
      fpf self.out {|@.@[<v2>let pp_%s out self = match self with|} name;
      List.iter
        (fun (c, _) ->
          fpf self.out {|@ | %s -> Format.fprintf out %S|} (mangle_cstor c)
            (mangle_cstor c))
        cases;
      fpf self.out {|@.|}
    );

    (* to_int *)
    fpf self.out {|@.@[<2>let int_of_%s = function@ |} name;
    List.iter
      (fun (c, n) -> fpf self.out "@ | %s -> %d" (mangle_cstor c) n)
      cases;
    fpf self.out {|@.|};

    (* of_int *)
    fpf self.out {|@.@[<v2>let %s_of_int = function|} name;
    List.iter
      (fun (c, n) -> fpf self.out "@ | %d -> %s" n (mangle_cstor c))
      cases;
    fpf self.out
      {|@ | n -> failwith (Printf.sprintf "unknown enum member %%d for `%s`" n)|}
      name;
    fpf self.out {|@.|};

    (* write *)
    fpf self.out "@.(** Serialize a %S *)@." name;
    fpf self.out {|@[<v2>let write_%s (module OP:PROTOCOL_WRITE) (self:%s) =@ |}
      name name;
    fpf self.out {|OP.write_i16 (int_of_%s self)|} name;
    fpf self.out {|@.|};

    (* TODO: write *)
    ()

  let cg_exception ~pp (self : t) name (fields : A.Field.t list) : unit =
    let name = mangle_cstor name in

    let pp_field out (f : A.Field.t) =
      fpf out "%s: %a" (mangle_name f.name) pp_ty f.ty
    in
    let pp_fields out (fs : A.Field.t list) =
      List.iteri
        (fun i f ->
          if i > 0 then fpf out ";@ ";
          pp_field out f)
        fs
    in

    (* type def *)
    fpf self.out {|@.@[<v2>exception %s|} name;
    if fields = [] then
      ()
    else
      fpf self.out " of {@;%a@;<1 -2>}" pp_fields fields;
    fpf self.out {|@]@.|};

    (* TODO: write *)
    ()

  (** Define (mutually recursive) types *)
  let cg_new_types ~pp (self : t) (defs : (_ * _ * Ast.Field.t list) list) :
      unit =
    let defs = List.map (fun (n, k, fs) -> mangle_name n, k, fs) defs in

    (* pair fields with their field_id *)
    let pair_with_field_id (fs : A.Field.t list) : (field_id * _) list =
      let cur_field = ref 1 in
      List.map
        (fun (f : A.Field.t) ->
          (* unique number for this field *)
          let f_id =
            match f.id with
            | None ->
              let n = !cur_field in
              incr cur_field;
              n
            | Some i ->
              cur_field := max !cur_field (i + 1);
              i
          in
          f_id, f)
        fs
    in

    let cg_def_field out (f : A.Field.t) =
      fpf out "%s: %a%s" (mangle_name f.name) pp_ty f.ty
        (if A.Field.is_required f then
          ""
        else
          " option")
    in
    let cg_def_field_def out (fs : A.Field.t list) =
      List.iteri
        (fun i f ->
          if i > 0 then fpf out ";@ ";
          cg_def_field out f)
        fs
    in

    (* define types *)
    let cg_def_type ~first out (name, k, fields) =
      if first then fpf out "type ";
      match k with
      | `Struct -> fpf out "%s = {@;%a@;<1 -2>}" name cg_def_field_def fields
      | `Union ->
        fpf out "%s =" name;
        List.iter
          (fun (f : A.Field.t) ->
            fpf out "@ | %s of %a" (mangle_cstor f.name) pp_ty f.ty)
          fields
    in

    fpf self.out {|@.@[<v>@[<v2>%a@]@]@.|} (pp_l_and cg_def_type) defs;

    (* TODO: build function, with default values *)

    (* printer *)
    let cg_def_pp ~first out (name, (k : [ `Union | `Struct ]), fields) =
      if first then
        fpf out "let rec pp_%s out (self:%s) =" name name
      else
        fpf out "pp_%s out (self:%s) =" name name;
      (match k with
      | `Union -> fpf out "@ match self with"
      | `Struct -> ());
      match k with
      | `Struct ->
        fpf out {|@ Format.fprintf out "{@@[";@ |};
        List.iter
          (fun (f : A.Field.t) ->
            let f_name = mangle_name f.name in
            if A.Field.is_required f then
              fpf out {|@[<2>Format.fprintf out "%s=%%a"@ %a self.%s@];@ |}
                f_name cg_printer_fun_for_ty f.ty f_name
            else
              fpf out
                "(@[<v>match self.%s with@ | None -> ()@ | @[Some x ->@ \
                 Format.fprintf out {|%s=%%a|}@ %a x@]@]);@ "
                f_name f_name cg_printer_fun_for_ty f.ty)
          fields;
        fpf out {|Format.fprintf out "@@]}"|}
      | `Union ->
        List.iter
          (fun (f : A.Field.t) ->
            fpf out
              {|@ | @[%s self ->@ Format.fprintf out "%s (%%a)"@ %a self@]|}
              (mangle_cstor f.name) (mangle_cstor f.name) cg_printer_fun_for_ty
              f.ty)
          fields
    in
    if pp then fpf self.out {|@.@[<v>@[<v2>%a@]@]@.|} (pp_l_and cg_def_pp) defs;

    (* writer *)
    let cg_writer ~first out (name, k, fields) =
      let fields_with_id = pair_with_field_id fields in

      if first then
        fpf out "let rec write_%s (module OP:PROTOCOL_WRITE) (self:%s) =@ " name
          name
      else
        fpf out "pp_%s (module OP:PROTOCOL_WRITE) (self:%s) =@ " name name;
      match k with
      | `Struct ->
        fpf out "let {%s} = self in@ "
          (String.concat "; " @@ List.map A.Field.name fields);
        fpf out "OP.write_struct_begin %S;@ " name;
        fpf out "%a;@ " cg_write_fields fields_with_id;
        fpf out "OP.write_struct_end ()"
      | `Union ->
        (* check all fields are required *)
        List.iter
          (fun (f : A.Field.t) ->
            if not (A.Field.is_required f) then
              failwith @@ spf "union needs required fields, %S is not" f.name)
          fields;
        (* emit code *)
        fpf out "OP.write_struct_begin %S;@ " name;
        fpf out "(@[<v>match self with";
        List.iter
          (fun ((field_id, f) : field_id * A.Field.t) ->
            fpf out "@ | @[%s x ->@ %a@]" (mangle_cstor f.name) cg_write_field
              ("x", field_id, f))
          fields_with_id;
        fpf out "@]);";
        fpf out "OP.write_struct_end ()";
        ()
      (* TODO
         List.iter
           (fun (f : A.Field.t) ->
             fpf out
               {|@ | @[%s self ->@ Format.fprintf out "%s (%%a)"@ %a self@]|}
               (mangle_cstor f.name) (mangle_cstor f.name) cg_printer_fun_for_ty
               f.ty)
           fields
      *)
    in
    fpf self.out "@.(** Serialize *)@.";
    fpf self.out {|@[<v>@[<v2>%a@]@]@.|} (pp_l_and cg_writer) defs;

    ()

  let cg_service (self : t) name ~extends funs : unit =
    let name = mangle_name name in

    (* def *)
    fpf self.out "@.(** Server-side for service %S *)@." name;
    fpf self.out "@[<v>class virtual server_%s = object (self)@ " name;

    (* inherit from extend *)
    Option.iter
      (fun e -> fpf self.out "inherit server_%s@ " (mangle_name e))
      extends;

    fpf self.out "  inherit service_any@ ";
    fpf self.out "  method name = %S@ " name;

    let cg_method (f : A.Function.t) =
      let f_name = mangle_name f.name in
      let pp_arg out (arg : A.Field.t) =
        let lbl =
          match arg.req with
          | A.Field.Optional | A.Field.Default -> "?"
          | A.Field.Required -> ""
        in
        fpf out "%s%s:%a" lbl arg.name pp_ty arg.ty
      in
      fpf self.out "@   method virtual %s : %a -> unit -> %a@ " f_name
        (pp_l ~sep:"->" pp_arg) f.args pp_fun_ty f.ty
    in

    List.iter cg_method funs;

    (* now generate the processor *)
    fpf self.out "@   (** Process an incoming message *)@ ";
    fpf self.out
      "  @[<v2>method process (ip:protocol_read) (op:protocol_write) : unit =@ ";
    fpf self.out "let (module IP) = ip in@ ";
    fpf self.out "let msg_name, msg_ty, seq_num = IP.read_msg_begin () in@ ";
    fpf self.out "IP.read_msg_end();@ ";
    fpf self.out "match msg_name, msg_ty with@ ";
    let emit_fun_case (f : A.Function.t) =
      let m_ty =
        if f.oneway then (
          (match f.ty with
          | A.Function_type.Void -> ()
          | A.Function_type.Ty ty ->
            failwith
            @@ Format.asprintf
                 "cannot have a oneway function %S with return type %a" f.name
                 A.Type.pp ty);
          "MSG_ONEWAY"
        ) else
          "MSG_CALL"
      in
      fpf self.out "| @[%S, %s ->@ " f.name m_ty;
      fpf self.out "assert false (* TODO *)@]@ "
      (* TODO: emit code to read struct making up arguments, binding them
         into references as we go;
         then emit [match self#<the method name> ?a1:!a1 ~a2:!a2 () with
           | ret -> write_msg_out; …
           | exception E1 (* thrown *) -> write_msg_out …
           | exception E2 (* thrown *) -> write_msg_out …
           | exception exn -> failwith "unhandled exception …"
         ]
      *)
    in
    List.iter emit_fun_case funs;
    fpf self.out
      "| _n, _ -> failwith (Printf.sprintf {|invalid message %%S|} _n)@]@ ";

    fpf self.out "@;<1 -2>end@]@.";
    ()

  (** does this produce an OCaml type definition? *)
  let is_newtype = function
    | A.Definition.{ view = Struct _ | Union _; _ } -> true
    | _ -> false

  let as_newtype_exn (d : A.Definition.t) =
    match d.view with
    | A.Definition.Struct { fields } -> d.name, `Struct, fields
    | A.Definition.Union { fields } -> d.name, `Union, fields
    | _ -> failwith "not a type definition"

  let encode_def_scc (self : t) ~pp (defs : A.Definition.t list) : unit =
    if !debug then
      Format.eprintf "codegen for defs [%s]@."
        (String.concat ";" @@ List.map (fun d -> d.A.Definition.name) defs);

    match defs with
    | [] -> assert false
    | [ A.Definition.{ name; view = Const { ty; value }; _ } ] ->
      cg_const self name ty value
    | [ A.Definition.{ name; view = TypeDef { ty } } ] ->
      cg_typedef ~pp self name ty
    | [ A.Definition.{ name; view = Enum { cases } } ] ->
      cg_enum ~pp self name cases
    | [ A.Definition.{ name; view = Exception { fields } } ] ->
      cg_exception ~pp self name fields
    | defs when List.for_all is_newtype defs ->
      cg_new_types ~pp self (List.map as_newtype_exn defs)
    | [ A.Definition.{ name; view = Service { extends; funs } } ] ->
      cg_service self name ~extends funs
    | defs ->
      failwith
      @@ Format.asprintf "cannot generate code for definitions %a"
           (CCFormat.Dump.list A.Definition.pp)
           defs

  let encode_header ~pp (h : A.Header.t) : unit = ()

  let encode_file (self : t) ~pp (file : A.File.t) : unit =
    let defs = Find_scc.top file in
    List.iter (encode_header ~pp) defs.headers;
    List.iter (encode_def_scc ~pp self) defs.def_scc_l
end

let parse_file f : A.File.t =
  if !debug then Printf.eprintf "parse file %S\n%!" f;
  let content = CCIO.File.read_exn f in
  match Parser.parse_string Parser.file content with
  | Error msg ->
    Format.eprintf "parse error: %s@." msg;
    exit 1
  | Ok x -> x

let codegen ~to_stdout ~out ~pp ~filename file : unit =
  let cg = CG.create () in
  CG.add_prelude ~filename cg;
  CG.encode_file cg ~pp file;
  if !debug then Printf.eprintf "generate code into %S\n" out;
  if out <> "" then (
    let oc = open_out out in
    CG.write_code oc cg;
    flush oc;
    close_out oc
  );
  if to_stdout then CG.write_code stdout cg;
  ()

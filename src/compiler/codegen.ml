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

  type t = {
    buf: Buffer.t;
    out: fmt;
    exns: (string, (int * A.Field.t) list) Hashtbl.t;
  }

  let fpf (self : fmt) fmt = Format.fprintf self fmt

  let create () : t =
    let buf = Buffer.create 1024 in
    let out = Format.formatter_of_buffer buf in
    { out; buf; exns = Hashtbl.create 32 }

  let prelude file =
    spf
      "(* generated from %S using smol_thrift codegen *)\n\
       [@@@ocaml.warning {|-26-27-39|}]\n\
       let pp_pair ppk ppv out (k,v) = Format.fprintf out {|(%%a,%%a)|} ppk k \
       ppv v\n\
       let pp_list ppx out l = Format.fprintf out {|[@[%%a@]]|}\n\
      \   (Format.pp_print_list ~pp_sep:(fun out () -> Format.fprintf out {|;@ \
       |}) ppx) l"
      file

  let add_prelude ~filename self =
    fpf self.out "%s@." (prelude filename);
    fpf self.out "open Thrifty.Types@.";
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

  let cg_read_field_ty_of_ty (ty : A.Type.t) =
    match ty.view with
    | A.Type.Named _ -> "T_STRUCT"
    | A.Type.List _ -> "T_LIST"
    | A.Type.Set _ -> "T_SET"
    | A.Type.Map _ -> "T_MAP"
    | A.Type.Base b ->
      (match b with
      | T_BOOL -> "T_BOOL"
      | T_BYTE | T_I8 -> "(T_BYTE | T_I8)"
      | T_I16 -> "T_I16"
      | T_I32 -> "T_I32"
      | T_I64 -> "T_I64"
      | T_DOUBLE -> "T_DOUBLE"
      | T_STRING | T_BINARY -> "(T_STRING | T_BINARY)"
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

  (** Generate a deserializer for [(module IP) (name:ty)] *)
  let rec cg_read_for_ty out (ty : A.Type.t) : unit =
    match ty.view with
    | Named s -> fpf out "read_%s (module IP) " (mangle_name s)
    | List ty ->
      let fty = cg_read_field_ty_of_ty ty in
      fpf out "(@[let _ty, len = IP.read_list_begin () in@ ";
      fpf out "assert (len=0 || match _ty with %s -> true | _ -> false);@ " fty;
      fpf out "@[<2>let l = List.init len@ (@[<hv>fun _i ->@ %a@])@] in@ "
        cg_read_for_ty ty;
      fpf out "IP.read_list_end();@ ";
      fpf out "l@])"
    | Set ty ->
      let fty = cg_read_field_ty_of_ty ty in
      fpf out "(@[let _ty, len = IP.read_set_begin () in@ ";
      fpf out "assert (len=0 || match _ty with %s -> true | _ -> false);@ " fty;
      fpf out "@[<2>let l = List.init len@ (@[<hv>fun _i ->@ %a@])@] in@ "
        cg_read_for_ty ty;
      fpf out "IP.read_set_end();@ ";
      fpf out "l@])"
    | Map (ty1, ty2) ->
      let fty1 = cg_read_field_ty_of_ty ty1 in
      let fty2 = cg_read_field_ty_of_ty ty2 in
      fpf out "(@[<v>let tyk, tyv, len = IP.read_map_begin () in@ ";
      fpf out
        "assert (len=0 || match tyk, tyv with %s,%s -> true | _ -> false);@ "
        fty1 fty2;
      fpf out
        "@[<2>List.init len@ (@[<hv>fun _i ->@ @[<2>let k =@ %a in@]@ @[<2>let \
         v =@ %a in@]@ k,v@])@]@])"
        cg_read_for_ty ty1 cg_read_for_ty ty2
    | Base b ->
      let m =
        match b with
        | T_BOOL -> "read_bool"
        | T_BYTE -> "read_byte"
        | T_I8 -> "read_i8"
        | T_I16 -> "read_i16"
        | T_I32 -> "read_i32"
        | T_I64 -> "read_i64"
        | T_DOUBLE -> "read_double"
        | T_STRING -> "read_string"
        | T_BINARY -> "read_binary"
        | T_STRUCT | T_MAP | T_SET | T_LIST ->
          failwith
          @@ Format.asprintf "bad base type %s" (string_of_field_type b)
      in
      fpf out "IP.%s ()" m

  (* code to write a field, whose value is stored in variable [var_name] *)
  let cg_write_field out ((var_name, field_id, f) : string * int * A.Field.t) :
      unit =
    let fty = cg_write_field_ty_of_ty f.ty in
    if A.Field.is_required f then
      fpf out
        "@[<v2>begin@ OP.write_field_begin %S %s %d;@ %a;@ \
         OP.write_field_end();@;\
         <1 -2>end@]" f.name fty field_id cg_write_for_ty (var_name, f.ty)
    else
      fpf out
        "(@[<v>match %s with@ | None -> ()@ | @[<v>Some x ->@ \
         OP.write_field_begin %S %s %d;@ %a;@ OP.write_field_end()@]@])"
        (mangle_name f.name) f.name fty field_id cg_write_for_ty ("x", f.ty)

  (* generate code to write fields into a [protocol_write].
     Each field is paired with its assigned field ID and the name
     of the variable containing its value,
     the protocol in module [OP] (output protocol) *)
  let cg_write_fields out (fs : (int * A.Field.t * string) list) : unit =
    let write_field i ((field_id, f, var_name) : int * A.Field.t * string) =
      if i > 0 then fpf out ";@ ";
      cg_write_field out (var_name, field_id, f)
    in
    (* by increasing field, helps with compact protocol *)
    let fs = List.sort (fun (i, _, _) (j, _, _) -> compare i j) fs in
    fpf out "@[<v2>begin@ ";
    List.iteri write_field fs;
    fpf out "@;<1 -2>end@]"

  (* read fields from [(module IP)], each field into a variable of the same
     name as the field. Optional/default fields will be of option type. *)
  let cg_read_fields out (fs : (int * A.Field.t) list) : unit =
    let gen_var ((_, f) : _ * A.Field.t) : unit =
      fpf out "let %s = ref %t in@ " (mangle_name f.name) (fun out ->
          match f.default with
          | None -> fpf out "None"
          | Some v ->
            fpf out "(@[Some (%a)@])" (pp_const_value ~ty:(Some f.ty)) v)
    and read_field ((field_id, f) : int * A.Field.t) =
      let ty_pat = cg_read_field_ty_of_ty f.ty in
      fpf out "| (%S, %s, _) | (_, %s, %d) ->@ " f.name ty_pat ty_pat field_id;
      fpf out "  @[<2>%s :=@ Some(@[%a@])@];@ " (mangle_name f.name)
        cg_read_for_ty f.ty;
      fpf out "| (%S, _, _) | (_, _, %d) ->@ " f.name field_id;
      fpf out
        "  @[<2>raise (Runtime_error@ (@[UE_invalid_protocol,@ {|invalid type \
         for field (%d: %S)|}@]))@]@ "
        field_id f.name
    in
    (* make a loop *)
    fpf out "let continue = ref true in@ ";
    List.iter gen_var fs;
    fpf out "@[<v2>while !continue do@ ";
    fpf out "match IP.read_field_begin () with@ ";
    fpf out
      "@[| exception Thrifty.Types.Read_stop_field -> continue := false@]@ ";
    List.iter read_field fs;
    fpf out "| _ -> () (* unknown field *)";
    fpf out "@;<1 -2>done@];@ ";
    ()

  let cg_typedef ~pp (self : t) name ty : unit =
    let name = mangle_name name in
    fpf self.out {|@.@[<2>type %s = %a@]@.|} name pp_ty ty;
    fpf self.out
      {|@.@[<v2>let write_%s (module OP:PROTOCOL_WRITE) (self:%s) : unit =@ %a@]@.|}
      name name cg_write_for_ty ("self", ty);
    fpf self.out
      {|@.@[<v2>let read_%s (module IP:PROTOCOL_READ) : %s =@ %a@]@.|} name name
      cg_read_for_ty ty;
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
      fpf self.out {|@.@[<v2>let pp_%s out self =@ |} name;
      fpf self.out "@[<hv2>let s = match self with";
      List.iter
        (fun (c, _) ->
          fpf self.out {|@ | %s -> %S|} (mangle_cstor c) (mangle_cstor c))
        cases;
      fpf self.out " in@]@ Format.fprintf out {|%%s|} s@."
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
      {|@ | n -> raise (Runtime_error (UE_invalid_protocol, Printf.sprintf "unknown enum member %%d for `%s`" n))|}
      name;
    fpf self.out {|@.|};

    (* write *)
    fpf self.out "@.(** Serialize a %S *)@." name;
    fpf self.out {|@[<v2>let write_%s (module OP:PROTOCOL_WRITE) (self:%s) =@ |}
      name name;
    fpf self.out {|OP.write_i16 (int_of_%s self)|} name;
    fpf self.out {|@.|};

    (* read *)
    fpf self.out "@.(** Deserialize a %S *)@." name;
    fpf self.out {|@[<v2>let read_%s (module IP:PROTOCOL_READ) : %s =@ |} name
      name;
    fpf self.out {|IP.read_i16 () |> %s_of_int|} name;
    fpf self.out {|@.|};

    ()

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

  (* read helper for a field: obtain a local variable named "foo" for
     field "foo", of optional type; if field is required, extract the option
     or else fail *)
  let cg_read_extract_var_of_field out ((_fid, f) : int * A.Field.t) =
    let fname = mangle_name f.name in
    if A.Field.is_required f then (
      fpf out "@[<hv2>let %s = match !%s with@ " fname fname;
      fpf out
        "| @[None ->@ raise (@[<2>Runtime_error@ (UE_invalid_protocol,@ \
         {|field (%d: %S) is required|})@])@]@ "
        _fid f.name;
      fpf out "| Some x -> x@] in@ "
    ) else
      fpf out "let %s = !%s in@ " fname fname

  let cg_exception (self : t) exn_name (fields : A.Field.t list) : unit =
    let exn_name = mangle_cstor exn_name in

    let fields_with_ids = pair_with_field_id fields in

    let pp_field out (f : A.Field.t) =
      fpf out "%s: %a" (mangle_name f.name) pp_ty f.ty;
      if A.Field.is_required f then
        ()
      else
        fpf out " option"
    in
    let pp_fields out (fs : A.Field.t list) =
      List.iteri
        (fun i f ->
          if i > 0 then fpf out ";@ ";
          pp_field out f)
        fs
    in

    (* type def *)
    fpf self.out {|@.@[<v2>exception %s|} exn_name;
    if fields = [] then
      ()
    else
      fpf self.out " of {@;%a@;<1 -2>}" pp_fields fields;
    fpf self.out {|@]@.|};

    (* remember the definition; reader/writer will be generated inline *)
    Hashtbl.add self.exns exn_name fields_with_ids;

    ()

  (** Define (mutually recursive) types *)
  let cg_new_types ~pp (self : t) (defs : (_ * _ * Ast.Field.t list) list) :
      unit =
    let defs = List.map (fun (n, k, fs) -> mangle_name n, k, fs) defs in

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
        fpf out
          "let rec write_%s (module OP:PROTOCOL_WRITE) (self:%s) : unit =@ "
          name name
      else
        fpf out "write_%s (module OP:PROTOCOL_WRITE) (self:%s) : unit =@ " name
          name;
      match k with
      | `Struct ->
        fpf out "let {%s} = self in@ "
          (String.concat "; " @@ List.map A.Field.name fields);
        fpf out "OP.write_struct_begin %S;@ " name;
        fpf out "%a;@ " cg_write_fields
          (List.map
             (fun (id, f) -> id, f, mangle_name f.A.Field.name)
             fields_with_id);
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

    (* reader *)
    let cg_reader ~first out (name, k, fields) =
      let fields_with_id = pair_with_field_id fields in

      if first then
        fpf out "let rec read_%s (module IP:PROTOCOL_READ) : %s =@ " name name
      else
        fpf out "read_%s (module IP:PROTOCOL_READ) : %s =@ " name name;
      match k with
      | `Struct ->
        fpf out "let _name = IP.read_struct_begin () in@ ";
        cg_read_fields out fields_with_id;
        fpf out "IP.read_struct_end ();@ ";
        List.iter (cg_read_extract_var_of_field out) fields_with_id;

        fpf out "{%s}"
          (String.concat ";"
          @@ List.map (fun f -> mangle_name f.A.Field.name) fields)
      | `Union ->
        fpf out "let _name = IP.read_struct_begin () in@ ";
        cg_read_fields out fields_with_id;
        fpf out "IP.read_struct_end ();@ ";
        fpf out "(* check which field is set *)@ ";
        fpf out "(@[<v>match %s with@ "
          (String.concat ", "
          @@ List.map
               (fun f -> Printf.sprintf "!%s" (mangle_name @@ A.Field.name f))
               fields);

        List.iteri
          (fun i (f : A.Field.t) ->
            let cname = mangle_cstor f.name in
            (* print pattern *)
            fpf out "| ";
            for j = 0 to List.length fields - 1 do
              if j > 0 then fpf out ",";
              if i = j then
                fpf out "Some x"
              else
                fpf out "_"
            done;
            fpf out " -> %s x@ " cname)
          fields;

        fpf out
          "| _ -> raise (Runtime_error (UE_protocol_error, Printf.sprintf {|no \
           field set for %S|}))@ "
          name;
        fpf out "@])"
    in

    fpf self.out "@.(** Deserialize *)@.";
    fpf self.out {|@[<v>@[<v2>%a@]@]@.|} (pp_l_and cg_reader) defs;

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

      if f.oneway then
        fpf self.out "@   @[method virtual %s :@ @[%a -> unit -> unit@]@]@ "
          f_name (pp_l ~sep:"->" pp_arg) f.args
      else
        fpf self.out
          "@   @[method virtual %s :@ @[%a -> unit -> %a \
           server_outgoing_reply@]@]@ "
          f_name (pp_l ~sep:"->" pp_arg) f.args pp_fun_ty f.ty
    in

    List.iter cg_method funs;

    (* now generate the processor *)
    fpf self.out "@   (** Process an incoming message *)@ ";
    fpf self.out
      "  @[<v2>method process (ip:protocol_read) ~(reply:(protocol_write -> \
       unit) -> unit) : unit =@ ";

    fpf self.out "let (module IP) = ip in@ ";

    fpf self.out "let msg_name, msg_ty, seq_num = IP.read_msg_begin () in@ ";
    fpf self.out "IP.read_msg_end();@ ";

    (* local helper: reply with success *)
    let cg_reply_success f =
      fpf self.out "reply @@@@ fun (module OP:PROTOCOL_WRITE) ->@ ";
      fpf self.out "OP.write_msg_begin {||} MSG_REPLY seq_num;@ ";

      fpf self.out "OP.write_msg_end();@ ";
      fpf self.out "OP.write_struct_begin {||};@ ";
      f ();
      fpf self.out "OP.write_field_stop();@ ";
      fpf self.out "OP.write_struct_end ()"
    in

    (* local helper:
       how to reply with an unexpected_exception and a message.
       the corresponding struct is "1: string message; 2: i32 type" *)
    fpf self.out "(* reply using a runtime failure *)@ ";
    fpf self.out
      "@[<v2>let reply_exn_ (ue:unexpected_exception) (msg:string) : unit =@ ";
    fpf self.out "reply @@@@ fun (module OP:PROTOCOL_WRITE) ->@ ";
    fpf self.out "OP.write_msg_begin {||} MSG_EXCEPTION seq_num;@ ";
    fpf self.out "OP.write_msg_end ();@ ";
    (* write fields *)
    fpf self.out "let ty = Thrifty.Types.int_of_unexpected_exception ue in@ ";
    cg_write_fields self.out
      [
        0, A.Field.field_rpc_exn_type, "ty"; 1, A.Field.field_rpc_exn_msg, "msg";
      ];
    fpf self.out "@;<1 -2>in@]@ ";

    (* guard against runtime errors here, now that we can reply (we have a
       sequence number and so {!reply_exn_} can be defined above) *)
    fpf self.out "try (@ ";

    fpf self.out "match msg_name, msg_ty with@ ";

    (* emit code to read struct making up arguments, binding them
       into references as we go;
       then emit [match self#<the method name> ?a1:!a1 ~a2:!a2 () with
         | ret -> write_msg_out; …
         | exception E1 (* thrown *) -> write_msg_out …
         | exception E2 (* thrown *) -> write_msg_out …
         | exception exn -> fail_ "unhandled exception …"
       ]
    *)
    let emit_fun_case (f : A.Function.t) =
      let fields_with_id = pair_with_field_id f.args in
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
      fpf self.out "| @[<v>%S, %s ->@ " f.name m_ty;
      fpf self.out "(* read arguments *)@ ";
      fpf self.out "let _name = IP.read_struct_begin () in@ ";
      cg_read_fields self.out fields_with_id;
      fpf self.out "IP.read_struct_end();@ ";
      List.iter (cg_read_extract_var_of_field self.out) fields_with_id;
      (* call method *)
      let args =
        String.concat " "
        @@ List.map
             (fun (f : A.Field.t) ->
               let pre =
                 if A.Field.is_required f then
                   "~"
                 else
                   "?"
               in
               spf "%s%s" pre (mangle_name f.name))
             f.args
      in
      if f.oneway then (
        (* just call method *)
        fpf self.out "(try self#%s %s () with _ -> ());" (mangle_name f.name)
          args;
        fpf self.out "@]@ "
      ) else (
        (* call method, get result, send it back (or send back exception) *)
        fpf self.out "@[<v2>let reply (x:_ result) : unit =@ ";
        fpf self.out "match x with";

        fpf self.out "@ | @[<v>Ok res ->@ ";
        cg_reply_success (fun () ->
            (* write field, if we return anything *)
            match f.ty with
            | A.Function_type.Void -> ()
            | A.Function_type.Ty ret ->
              cg_write_field self.out ("res", 0, A.Field.field_rpc_success ret);
              fpf self.out ";@ ");

        fpf self.out "@]";

        (* generate code for declared exception *)
        let cg_throw (exn_field : A.Field.t) : unit =
          let exn_name, exn_fields_with_ids =
            match exn_field.ty.view with
            | A.Type.Named n ->
              let name = mangle_cstor n in
              let fs =
                try Hashtbl.find self.exns name
                with Not_found ->
                  failwith (spf "cannot find definition of exn %S" name)
              in
              name, fs
            | _ -> failwith "cannot throw anything but an exception"
          and exn_f_id =
            match exn_field.id with
            | None ->
              failwith
                (spf "cannot have field %S in `throws` without explicit ID"
                   exn_field.name)
            | Some i -> i
          in

          let exn_fs =
            List.map
              (fun (f_id, f) -> f_id, f, mangle_name f.A.Field.name)
              exn_fields_with_ids
          in

          (* match against exception *)
          if exn_fs = [] then
            fpf self.out "@ | @[<v>Error %s ->@ " exn_name
          else
            fpf self.out "@ | @[<v>exception (%s {%s}) ->@ " exn_name
              (String.concat ";" @@ List.map (fun (_, _, n) -> n) exn_fs);

          (* write fields in a single-field struct *)
          cg_reply_success (fun () ->
              fpf self.out "reply @@@@ fun (module OP:PROTOCOL_WRITE) ->@ ";
              fpf self.out "OP.write_field_begin {|%s|} T_STRUCT %d;@ "
                (mangle_name exn_field.name)
                exn_f_id;
              if exn_fs <> [] then (
                cg_write_fields self.out exn_fs;
                fpf self.out ";@ "
              );
              fpf self.out "OP.write_field_stop();@ ";
              fpf self.out "OP.write_field_end();@ ");
          fpf self.out "@]"
        in

        (* handle "throws", if present *)
        (match f.throws with
        | None -> ()
        | Some exns -> List.iter cg_throw exns);

        (* unhandled exceptions *)
        fpf self.out "@ | @[<v>Error exn ->@ ";
        fpf self.out
          "raise (Runtime_error (UE_internal_error, (Printexc.to_string \
           exn)))@]";

        (* end of reply function *)
        fpf self.out "@;<1 -2>@] in@ ";

        (* call user code with [reply] as continuation *)
        fpf self.out "(* call the user code *)@ ";
        fpf self.out "(@[<v>try self#%s %s () ~reply" (mangle_name f.name) args;
        fpf self.out "@ with e -> reply (Error e)";
        fpf self.out "@])@]@ "
      )
    in
    List.iter emit_fun_case funs;
    fpf self.out "| _n, _ -> ";
    fpf self.out
      "raise (Runtime_error (UE_invalid_message_type, {|invalid message|}));@ ";

    (* catch runtime errors and send them back as MSG_EXCEPTION *)
    fpf self.out "@[<v2>) with Runtime_error (ue, msg) ->@ ";
    fpf self.out "(* catch runtime errors and reify them *)@ ";
    fpf self.out "reply_exn_ ue msg;@]@]";
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
    | [ A.Definition.{ name; view = TypeDef { ty }; _ } ] ->
      cg_typedef ~pp self name ty
    | [ A.Definition.{ name; view = Enum { cases }; _ } ] ->
      cg_enum ~pp self name cases
    | [ A.Definition.{ name; view = Exception { fields }; _ } ] ->
      cg_exception self name fields
    | defs when List.for_all is_newtype defs ->
      cg_new_types ~pp self (List.map as_newtype_exn defs)
    | [ A.Definition.{ name; view = Service { extends; funs }; _ } ] ->
      cg_service self name ~extends funs
    | defs ->
      failwith
      @@ Format.asprintf "cannot generate code for definitions %a"
           (CCFormat.Dump.list A.Definition.pp)
           defs

  (* TODO: what to do there? *)
  let encode_header ~pp:_ (_h : A.Header.t) : unit = ()

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

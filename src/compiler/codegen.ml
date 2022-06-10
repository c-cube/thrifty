module A = Ast

let fpf = Format.fprintf
let spf = Printf.sprintf
let debug = ref false

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
       ppv v\n"
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
      fpf out "Format.pp_print_list@ %a@ out self" cg_printer_fun_for_ty ty
    | Map (ty1, ty2) ->
      fpf out "Format.pp_print_list@ (@[pp_pair@ %a@ %a@])@ out self"
        cg_printer_fun_for_ty ty1 cg_printer_fun_for_ty ty2
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
    | List ty | Set ty ->
      fpf out "(@[Format.pp_print_list@ %a@])" cg_printer_fun_for_ty ty
    | Map (ty1, ty2) ->
      fpf out "(@[Format.pp_print_list@ (@[pp_pair@ %a@ %a@])@])"
        cg_printer_fun_for_ty ty1 cg_printer_fun_for_ty ty2
    | _ -> fpf out "(@[fun out self ->@ %a@])" cg_printer_for_ty ty

  let cg_typedef ~pp (self : t) name ty : unit =
    let name = mangle_name name in
    fpf self.out {|@.@[<2>type %s = %a@]@.|} name pp_ty ty;
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

    ()

  (* print with "and" as separator *)
  let pp_l_and ppx out l =
    List.iteri
      (fun i x ->
        if i > 0 then fpf out "@]@ @[<v2>and ";
        ppx ~first:(i = 0) out x)
      l

  (** Define (mutually recursive) types *)
  let cg_new_types ~pp (self : t) (defs : (_ * _ * Ast.Field.t list) list) :
      unit =
    let defs = List.map (fun (n, k, fs) -> mangle_name n, k, fs) defs in

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

    (* define types *)
    let cg_def_type ~first out (name, k, fields) =
      if first then
        fpf out "%s"
          (match k with
          | `Struct -> "type "
          | `Union -> "union "
          | `Exception -> "exception ");
      match k with
      | `Struct -> fpf out "%s = {@;%a@;<1 -2>}" name pp_fields fields
      | `Exception when fields = [] -> fpf out "%s" name
      | `Exception -> fpf out "%s of {@;%a@;<1 -2>}" name pp_fields fields
      | `Union ->
        fpf out "%s = " name;
        List.iter
          (fun (f : A.Field.t) ->
            fpf out "@ | %s of %a" (mangle_cstor f.name) pp_ty f.ty)
          fields
    in

    fpf self.out {|@.@[<v>@[<v2>%a@]@]@.|} (pp_l_and cg_def_type) defs;

    (* printer *)
    let cg_def_pp ~first out (name, k, fields) =
      if first then
        fpf out "let rec pp_%s out (self:%s) = " name name
      else
        fpf out "pp_%s out (self:%s) = " name name;
      (match k with
      | `Exception when fields = [] -> ()
      | `Exception -> fpf out "let (%s self) = self in@ " (mangle_cstor name)
      | `Union -> fpf out "match self with@ "
      | `Struct -> ());
      match k with
      | `Struct ->
        fpf out {|Format.fprintf out "{@@[";@ |};
        List.iter
          (fun (f : A.Field.t) ->
            fpf out {|@[<2>Format.fprintf out "%s=%%a"@ %a self.%s@];@ |}
              (mangle_name f.name) cg_printer_fun_for_ty f.ty
              (mangle_name f.name))
          fields;
        fpf out {|Format.fprintf out "@@]}";|}
      | `Exception when fields = [] ->
        fpf out {|Format.fprintf out "%S"|} (mangle_cstor name)
      | `Exception ->
        fpf out {|Format.fprintf out "%S {@@[";@ |} (mangle_cstor name);
        List.iter
          (fun (f : A.Field.t) ->
            fpf out {|@[<2>Format.fprintf out "%s=%%a"@ %a self.%s@];@ |}
              (mangle_name f.name) cg_printer_fun_for_ty f.ty
              (mangle_name f.name))
          fields;
        fpf out {|Format.fprintf out "@@]}";|}
      | `Union ->
        List.iter
          (fun (f : A.Field.t) ->
            fpf out {|@ | %s self -> Format.fprintf out "%S (%%a)" %a self|}
              (mangle_cstor f.name) (mangle_cstor f.name) cg_printer_fun_for_ty
              f.ty)
          fields
    in
    if pp then fpf self.out {|@.@[<v>@[<v2>%a@]@]@.|} (pp_l_and cg_def_pp) defs;
    ()

  let cg_service (self : t) name ~extends funs : unit =
    let name = mangle_name name in
    (* def *)
    fpf self.out "@.@[<v2>class virtual server_%s = object@ " name;

    (* inherit from extend *)
    Option.iter
      (fun e -> fpf self.out "inherit server_%s@ " (mangle_name e))
      extends;

    List.iter
      (fun (f : A.Function.t) ->
        fpf self.out "(* todo: method %S *)@ " f.name;
        ())
      funs;

    (* TODO: emit "process : protocol_in -> protocol_out -> unit" based
       on the name + methods *)
    fpf self.out "@;<1 -2>end@]@.";
    ()

  (*

  (* codegen type definition.
     root: is [ty] directly at the top of a definition
     clique: other types in the same mutually-recursive clique as [ty] *)
  let rec cg_ty_ty ~root ~clique (self : fmt) (ty : Ast.File.t) : unit =
    let recurse = cg_ty_ty ~root:false ~clique in
    match ty with
    | A.Named_ty { name; _ } ->
      if List.mem name clique then
        fpf self "%s" (String.uncapitalize_ascii name)
      else
        fpf self "%s.t" (String.capitalize_ascii name)
    | A.Uint | A.Int -> addstr self "int64"
    | A.U8 | A.I8 -> addstr self "char"
    | A.U16 | A.I16 -> addstr self "int"
    | A.U32 | A.I32 -> addstr self "int32"
    | A.U64 | A.I64 -> addstr self "int64"
    | A.F32 | A.F64 -> addstr self "float"
    | A.Bool -> addstr self "bool"
    | A.String -> addstr self "string"
    | A.Data _ -> addstr self "bytes"
    | A.Void -> addstr self "unit"
    | A.Optional ty -> fpf self "%a option" recurse ty
    | A.Array { ty; len = _ } -> fpf self "@[%a@ array@]" recurse ty
    | A.Map (String, b) -> fpf self "@[%a@ Bare.String_map.t@]" recurse b
    | A.Map (a, b) -> fpf self "@[(@[%a *@ %a@]) list@]" recurse a recurse b
    | A.Struct l ->
      assert root;
      (* flattened *)
      fpf self "{@,";
      List.iteri
        (fun i (name, ty) ->
          if i > 0 then fpf self "@ ";
          fpf self "%s: %a;" name recurse ty)
        l;
      fpf self "@;<0 -2>}"

  (* named for the i-th element of an union *)
  let union_elt_name ~ty_name i (ty : A.ty_expr) : string =
    match ty with
    | Named_ty { name; _ } -> String.capitalize_ascii name
    | _ -> spf "%s_%d" (String.capitalize_ascii ty_name) i

  (* for [enum name l], produce int64<->t conversions *)
  let cg_enum_conv self _name l : unit =
    fpf self "@,@[<hv2>let to_int = function@,";
    (let n = ref 0 in
     List.iter
       (function
         | name, None ->
           fpf self "| @[%s ->@ %dL@]@," (String.capitalize_ascii name) !n;
           incr n
         | name, Some i ->
           assert (i >= !n);
           fpf self "| @[%s -> %dL@]@," (String.capitalize_ascii name) i;
           n := i + 1)
       l;
     fpf self "@]");
    fpf self "@,@[<hv2>let of_int = function@,";
    (let n = ref 0 in
     List.iter
       (function
         | name, None ->
           fpf self "| @[%dL ->@ %s@]@," !n (String.capitalize_ascii name);
           incr n
         | name, Some i ->
           assert (i >= !n);
           fpf self "| @[%dL ->@ %s@]@," i (String.capitalize_ascii name);
           n := i + 1)
       l;
     fpf self
       "| @[x -> invalid_arg@ (Printf.sprintf \"unknown enum member for %s.t: \
        %%Ld\" x)@]@]@,"
       (String.capitalize_ascii _name));
    ()

  (* codegen for type definition of this type def *)
  let cg_ty_def_rhs_def ~lead ~self_ty ~clique ty_name self (tyd : A.ty_def_rhs)
      : unit =
    match tyd with
    | A.Atomic ty ->
      fpf self "@[<v2>%s %s = %a@]@," lead self_ty
        (cg_ty_ty ~clique ~root:true)
        ty
    | A.Enum l ->
      fpf self "@[<hv2>%s %s =@ " lead self_ty;
      List.iteri
        (fun i (n, _) ->
          if i > 0 then fpf self "@ | ";
          addstr self (String.capitalize_ascii n))
        l;
      fpf self "@]@ "
    | A.Union l ->
      fpf self "@[<v2>%s %s =@ " lead self_ty;
      List.iteri
        (fun i ty ->
          let name = union_elt_name ~ty_name i ty in
          match ty with
          | Named_ty { is_void = true; _ } | Void -> fpf self "| %s@ " name
          | _ ->
            fpf self "| @[%s of %a@]@ " name (cg_ty_ty ~clique ~root:false) ty)
        l;
      fpf self "@]@,"

  (* codegen for decoding *)
  let rec cg_ty_decode ~root ~clique ~ty_name (self : fmt) (ty : A.ty_expr) :
      unit =
    let recurse = cg_ty_decode ~clique ~root:false ~ty_name in
    match ty with
    | A.Named_ty { name; is_void = true } ->
      (* refer to the constructor instead *)
      let cstor = union_elt_name ~ty_name:name (-1) ty in
      addstr self cstor
    | A.Named_ty { name; _ } ->
      if List.mem name clique then
        (* use the recursion callback *)
        fpf self "!_decode_%s dec" (String.uncapitalize_ascii name)
      else
        fpf self "%s.decode dec" (String.capitalize_ascii name)
    | A.Uint -> addstr self "Bare.Decode.uint dec"
    | A.Int -> addstr self "Bare.Decode.int dec"
    | A.U8 -> addstr self "Bare.Decode.i8 dec"
    | A.I8 -> addstr self "Bare.Decode.u8 dec"
    | A.U16 -> addstr self "Bare.Decode.u16 dec"
    | A.I16 -> addstr self "Bare.Decode.i16 dec"
    | A.U32 -> addstr self "Bare.Decode.u32 dec"
    | A.I32 -> addstr self "Bare.Decode.i32 dec"
    | A.U64 -> addstr self "Bare.Decode.u64 dec"
    | A.I64 -> addstr self "Bare.Decode.i64 dec"
    | A.F32 -> addstr self "Bare.Decode.f32 dec"
    | A.F64 -> addstr self "Bare.Decode.f64 dec"
    | A.Bool -> addstr self "Bare.Decode.bool dec"
    | A.String -> addstr self "Bare.Decode.string dec"
    | A.Data { len = None } -> addstr self "Bare.Decode.data dec"
    | A.Data { len = Some n } -> fpf self "Bare.Decode.data_of ~size:%d dec" n
    | A.Void -> addstr self "()"
    | A.Optional ty ->
      fpf self "@[<2>Bare.Decode.optional@ (@[fun dec ->@ %a@]) dec@]" recurse
        ty
    | A.Array { ty; len = Some len } ->
      fpf self "@[<2>Array.init %d@ (@[fun _ ->@ %a@])@]" len recurse ty
    | A.Array { ty; len = None } ->
      fpf self
        "(@[<v>let len = Bare.Decode.uint dec in@ if len>Int64.of_int \
         Sys.max_array_length then invalid_arg \"array too big\";@ \
         @[<2>Array.init (Int64.to_int len)@ (@[fun _ -> %a@])@]@])"
        recurse ty
    | A.Map (String, b) ->
      fpf self
        "(@[<v>let len = Bare.Decode.uint dec in@ if len>Int64.of_int max_int \
         then invalid_arg \"array too big\";@ @[<2>List.init (Int64.to_int \
         len)@ (@[<v>fun _ ->@ let k = Bare.Decode.string dec in@ let v = %a \
         in@ k,v@])@]@ |> List.to_seq |> Bare.String_map.of_seq@])"
        recurse b
    | A.Map (a, b) ->
      fpf self
        "(@[<v>let len = Bare.Decode.uint dec in@ if len>Int64.of_int \
         Sys.max_array_length then invalid_arg \"array too big\";@ \
         @[<2>List.init (Int64.to_int len)@ (@[fun _ ->@ let k = %a in@ let v \
         = %a@ in k,v@])@]@])"
        recurse a recurse b
    | A.Struct l ->
      assert root;
      (* flattened *)
      fpf self "@[<hv>";
      List.iter
        (fun (n, ty) -> fpf self "@[<2>let %s =@ %a in@]@ " n recurse ty)
        l;
      fpf self "{@[<hv>";
      List.iter (fun (n, _) -> fpf self "%s;@ " n) l;
      fpf self "@]}@]"

  (* codegen for decoding *)
  let cg_ty_def_rhs_decode ~clique ty_name (self : fmt) (def : A.ty_def_rhs) :
      unit =
    match def with
    | A.Atomic ty -> cg_ty_decode ~clique ~root:true ~ty_name self ty
    | A.Enum _ -> fpf self "of_int (Bare.Decode.uint dec)"
    | A.Union l ->
      fpf self "let tag = Bare.Decode.uint dec in@ match tag with@ ";
      List.iteri
        (fun i ty ->
          let cstor = union_elt_name ~ty_name i ty in
          match ty with
          | Named_ty { is_void = true; _ } | Void ->
            (* nullary *)
            fpf self "| @[%dL ->@ %s@]@ " i cstor
          | _ ->
            fpf self "| @[%dL ->@ %s (%a)@]@ " i cstor
              (cg_ty_decode ~clique ~root:false ~ty_name)
              ty)
        l;
      fpf self
        "| @[_ -> invalid_arg@ (Printf.sprintf \"unknown union tag %s.t: \
         %%Ld\" tag)@]@,"
        ty_name

  (* codegen for encoding [x] into [enc] *)
  let rec cg_ty_encode (x : string) ~clique ~root ~ty_name (self : fmt)
      (ty : A.ty_expr) : unit =
    let recurse x = cg_ty_encode ~clique ~root:false ~ty_name x in
    match ty with
    | A.Named_ty { name; _ } ->
      if List.mem name clique then
        (* use the recursion callback *)
        fpf self "!_encode_%s enc %s" (String.uncapitalize_ascii name) x
      else
        fpf self "%s.encode enc %s" (String.capitalize_ascii name) x
    | A.Uint -> fpf self "Bare.Encode.uint enc %s" x
    | A.Int -> fpf self "Bare.Encode.int enc %s" x
    | A.U8 -> fpf self "Bare.Encode.i8 enc %s" x
    | A.I8 -> fpf self "Bare.Encode.u8 enc %s" x
    | A.U16 -> fpf self "Bare.Encode.u16 enc %s" x
    | A.I16 -> fpf self "Bare.Encode.i16 enc %s" x
    | A.U32 -> fpf self "Bare.Encode.u32 enc %s" x
    | A.I32 -> fpf self "Bare.Encode.i32 enc %s" x
    | A.U64 -> fpf self "Bare.Encode.u64 enc %s" x
    | A.I64 -> fpf self "Bare.Encode.i64 enc %s" x
    | A.F32 -> fpf self "Bare.Encode.f32 enc %s" x
    | A.F64 -> fpf self "Bare.Encode.f64 enc %s" x
    | A.Bool -> fpf self "Bare.Encode.bool enc %s" x
    | A.String -> fpf self "Bare.Encode.string enc %s" x
    | A.Data { len = None } -> fpf self "Bare.Encode.data enc %s" x
    | A.Data { len = Some n } ->
      fpf self
        "(@[assert (Bytes.length %s=%d);@ Bare.Encode.data_of ~size:%d enc \
         %s@])"
        x n n x
    | A.Void -> fpf self "()"
    | A.Optional ty ->
      fpf self "@[<2>Bare.Encode.optional@ (@[fun enc xopt ->@ %a@]) enc %s@]"
        (recurse "xopt") ty x
    | A.Array { ty; len = Some len } ->
      fpf self
        "(@[<2>assert (Array.length %s = %d);@ Array.iter (@[fun xi ->@ %a@])@ \
         %s@])"
        x len (recurse "xi") ty x
    | A.Array { ty; len = None } ->
      fpf self
        "(@[<v>let arr = %s in@ Bare.Encode.uint enc (Int64.of_int \
         (Array.length arr));@ @[Array.iter (@[fun xi ->@ %a@])@ arr@]@])"
        x (recurse "xi") ty
    | A.Map (String, b) ->
      fpf self
        "(@[<v>Bare.Encode.uint enc (Int64.of_int (Bare.String_map.cardinal \
         %s));@ @[<2>Bare.String_map.iter@ (@[fun x y ->@ Bare.Encode.string \
         enc x;@ %a@])@ %s@]@])"
        x (recurse "y") b x
    | A.Map (a, b) ->
      fpf self
        "(@[<v>Bare.Encode.uint enc (Int64.of_int (List.length %s));\n\
        \                @[<2>List.iter@ (@[fun (x,y) ->@ %a;@ %a@])@ %s@]@])" x
        (recurse "x") a (recurse "y") b x
    | A.Struct l ->
      assert root;
      (* flattened *)
      fpf self "@[<hv2>begin@ ";
      List.iteri
        (fun i (n, ty) ->
          if i > 0 then fpf self "@ ";
          let field = spf "%s.%s" x n in
          fpf self "%a;" (recurse field) ty)
        l;
      fpf self "@;<1 -2>end@]";
      ()

  (* codegen for encoding *)
  let cg_ty_def_rhs_encode ty_name ~clique (self : fmt) (def : A.ty_def_rhs) :
      unit =
    match def with
    | A.Atomic ty -> cg_ty_encode "self" ~clique ~root:true ~ty_name self ty
    | A.Enum _ -> fpf self "Bare.Encode.uint enc (to_int self)"
    | A.Union l ->
      fpf self "@[<hv>match self with@ ";
      List.iteri
        (fun i ty ->
          let cstor = union_elt_name ~ty_name i ty in
          match ty with
          | A.Void | A.Named_ty { is_void = true; _ } ->
            fpf self "| @[<v>%s ->@ Bare.Encode.uint enc %dL@]@," cstor i
          | _ ->
            fpf self "| @[<v>%s x ->@ Bare.Encode.uint enc %dL;@ %a@]@," cstor i
              (cg_ty_encode ~clique ~root:false ~ty_name "x")
              ty)
        l

  (* define encoding/decoding/annex functions for [def] *)
  let cg_ty_encode_decode ~clique name self def : unit =
    (match def with
    | A.Enum l -> cg_enum_conv self name l
    | _ -> ());
    fpf self
      "@,\
       (** @raise Invalid_argument in case of error. *)@,\
       @[<v2>let decode (dec: Bare.Decode.t) : t =@ %a@]@,"
      (cg_ty_def_rhs_decode ~clique name)
      def;
    fpf self
      "@,@[<v2>let encode (enc: Bare.Encode.t) (self: t) : unit =@ %a@]@,"
      (cg_ty_def_rhs_encode ~clique name)
      def;
    ()

  (* codegen for a pretty printer [<ty> Bare.Pp.printer] *)
  let rec cg_pp_ty ~root ~clique name out (ty : A.ty_expr) : unit =
    let recurse = cg_pp_ty ~root:false ~clique name in
    match ty with
    | A.Named_ty { name; _ } ->
      if List.mem name clique then
        (* use the recursion callback *)
        fpf out "!_pp_%s" (String.uncapitalize_ascii name)
      else
        fpf out "%s.pp" (String.capitalize_ascii name)
    | A.Uint -> fpf out "Bare.Pp.int64"
    | A.Int -> fpf out "Bare.Pp.int64"
    | A.U8 -> fpf out "Bare.Pp.int8"
    | A.I8 -> fpf out "Bare.Pp.int8"
    | A.U16 -> fpf out "Bare.Pp.int"
    | A.I16 -> fpf out "Bare.Pp.int"
    | A.U32 -> fpf out "Bare.Pp.int32"
    | A.I32 -> fpf out "Bare.Pp.int32"
    | A.U64 -> fpf out "Bare.Pp.int64"
    | A.I64 -> fpf out "Bare.Pp.int64"
    | A.F32 -> fpf out "Bare.Pp.float"
    | A.F64 -> fpf out "Bare.Pp.float"
    | A.Bool -> fpf out "Bare.Pp.bool"
    | A.String -> fpf out "Bare.Pp.string"
    | A.Data { len = _ } -> fpf out "Bare.Pp.data"
    | A.Void -> fpf out "Bare.Pp.unit"
    | A.Optional ty -> fpf out "(@[<2>Bare.Pp.option@ %a@])" recurse ty
    | A.Array { ty; len = _ } -> fpf out "(@[<2>Bare.Pp.array@ %a@])" recurse ty
    | A.Map (String, b) ->
      fpf out
        "(@[<v>fun out map ->@ Bare.Pp.iter@ (@[fun out (xi,yi) ->@ \
         @[<2>Format.fprintf out \"(%%a -> %%a)\"@ %a xi@ %a yi@]@])@ out@ \
         (@[fun f ->@ Bare.String_map.iter (fun x y->f (x,y)) map@])@])"
        recurse String recurse b
    | A.Map (a, b) ->
      fpf out
        "(@[<v>Bare.Pp.list@ (@[fun out (xi,yi) -> Format.fprintf out \"(%%a \
         -> %%a)\" %a xi %a yi@])@])"
        recurse a recurse b
    | A.Struct l ->
      assert root;
      (* flattened *)
      fpf out
        "(@[<v2>@[<v>fun out x ->@ begin@]@ Format.fprintf out \"{ @@[\";@ ";
      List.iter
        (fun (name, ty) ->
          let field = spf "x.%s" name in
          fpf out "@[<2>Format.fprintf out \"%s=%%a;@@ \"@ %a@ %s@];@," name
            recurse ty field)
        l;
      fpf out "Format.fprintf out \"@@]}\";@;<1 -2>end@]@])";
      ()

  let cg_pp_def_rhs_encode ~clique name out (def : A.ty_def_rhs) : unit =
    match def with
    | A.Atomic ty -> fpf out "%a out self" (cg_pp_ty ~clique ~root:true name) ty
    | A.Enum cases ->
      fpf out "@[<v>match self with@ ";
      List.iter
        (fun (s, _) ->
          let c = String.capitalize_ascii s in
          fpf out "| @[%s ->@ Format.fprintf out %S@]@," c c)
        cases;
      fpf out "@]"
    | A.Union l ->
      fpf out "@[<hv>match self with@ ";
      List.iteri
        (fun i ty ->
          let cstor = union_elt_name ~ty_name:name i ty in
          match ty with
          | A.Void | A.Named_ty { is_void = true; _ } ->
            fpf out "| @[<v>%s ->@ Format.fprintf out %S@]@," cstor cstor
          | _ ->
            fpf out
              "| @[<v>%s x ->@ Format.fprintf out \"(@@[%s@@ %%a@@])\" %a x@]@,"
              cstor cstor
              (cg_pp_ty ~clique ~root:true name)
              ty)
        l

  (* define pretty printer for [def] *)
  let cg_pp ~clique name out def : unit =
    fpf out "@,@[<v2>let pp out (self:t) : unit =@ %a@]@,"
      (cg_pp_def_rhs_encode ~clique name)
      def


  *)

  let is_newtype = function
    | A.Definition.{ view = Struct _ | Exception _ | Union _; _ } -> true
    | _ -> false

  let as_newtype_exn (d : A.Definition.t) =
    match d.view with
    | A.Definition.Struct { fields } -> d.name, `Struct, fields
    | A.Definition.Union { fields } -> d.name, `Union, fields
    | A.Definition.Exception { fields } -> d.name, `Exception, fields
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
    | defs when List.for_all is_newtype defs ->
      cg_new_types ~pp self (List.map as_newtype_exn defs)
    | [ A.Definition.{ name; view = Service { extends; funs } } ] ->
      cg_service self name ~extends funs
    | defs ->
      failwith
      @@ Format.asprintf "cannot generate code for definitions %a"
           (CCFormat.Dump.list A.Definition.pp)
           defs
  (* TODO
     let { A.name; def } = d in
     if !debug then Format.eprintf "codegen for type %s@." name;
     fpf self.out "@[<v2>module %s = struct@," (String.capitalize_ascii name);
     cg_ty_def_rhs_def ~lead:"type" ~self_ty:"t" name ~clique:[ name ] self.out
       def;
     fpf self.out "%a" (cg_ty_encode_decode ~clique:[ name ] name) def;
     if pp then fpf self.out "%a" (cg_pp ~clique:[ name ] name) def;
     fpf self.out "@]@.end@.@.";
  *)

  (* TODO
     | defs ->
       (* first, declare all types in a mutually recursive block *)
       let clique = List.map (fun d -> d.A.name) defs in
       if !debug then
         Format.eprintf "codegen for types [%s]@." (String.concat "," clique);
       fpf self.out "@[<v>";
       List.iteri
         (fun i { A.name; def } ->
           let lead =
             if i = 0 then
               "type"
             else
               "and"
           in
           let self_ty = String.uncapitalize_ascii name in
           cg_ty_def_rhs_def ~lead ~self_ty name ~clique self.out def)
         defs;
       fpf self.out "@]@,";
       (* forward declarations for the mutually recursive functions *)
       List.iter
         (fun { A.name; _ } ->
           let self_ty = String.uncapitalize_ascii name in
           fpf self.out "let _encode_%s = ref (fun _ _ -> assert false)@,"
             self_ty;
           fpf self.out "let _decode_%s = ref (fun _ -> assert false)@," self_ty;
           if pp then
             fpf self.out "let _pp_%s = ref (fun _ _ -> assert false)@," self_ty)
         defs;
       (* now build one module for each type *)
       List.iter
         (fun { A.name; def } ->
           fpf self.out "@[<v2>module %s = struct@,"
             (String.capitalize_ascii name);
           (* alias+redeclare type *)
           let self_ty = String.uncapitalize_ascii name in
           let self_ty' = spf "t = %s" self_ty in
           cg_ty_def_rhs_def ~lead:"type" ~self_ty:self_ty' name ~clique self.out
             def;
           fpf self.out "%a" (cg_ty_encode_decode ~clique name) def;
           if pp then fpf self.out "%a" (cg_pp ~clique name) def;
           (* fill forward references *)
           fpf self.out "@,(* fill forward declarations *)@,";
           fpf self.out "let () = _encode_%s := encode@," self_ty;
           fpf self.out "let () = _decode_%s := decode@," self_ty;
           if pp then fpf self.out "let () = _pp_%s := pp@," self_ty;
           fpf self.out "@]@.end@.@.")
         defs;
       ()
  *)

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

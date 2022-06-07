module Fmt = CCFormat

let fpf = Fmt.fprintf
let pp_list_sp ppx = Fmt.list ~sep:(fun out () -> fpf out "@ ") ppx

module Const_value = struct
  type t =
    | Bool of bool
    | Int of int64
    | Double of float
    | String of string
    | List of t list
    | Map of (t * t) list
    | Named of string

  let rec pp out = function
    | Bool b -> Fmt.pp_print_bool out b
    | Int i -> fpf out "%Ld" i
    | Double f -> fpf out "%f" f
    | String s -> fpf out "%S" s
    | List l -> Fmt.Dump.list pp out l
    | Map l ->
      let pp_pair out (k, v) = fpf out "@[%a: %a@];" pp k pp v in
      fpf out "{@[%a@]}" (pp_list_sp pp_pair) l
    | Named s -> fpf out "%s" s
end

type identifier = string

module Metadata = struct
  type key = identifier
  type value = string (* literal *)

  type t = (key * value) list

  let pp out (self : t) =
    let pp_kv out (k, v) = fpf out " (@[%s = '%s'@])" k v in
    List.iter (pp_kv out) self
end

module Type = struct
  type t = { view: view; meta: Metadata.t }

  and view =
    | Base of field_type
    | Named of identifier
    | List of t
    | Map of t * t
    | Set of t

  let rec pp out self =
    (match self.view with
    | Base n -> Fmt.string out (string_of_field_type n)
    | Named s -> Fmt.string out s
    | List l -> fpf out "list< @[%a@] >" pp l
    | Set l -> fpf out "set< @[%a@] >" pp l
    | Map (a, b) -> fpf out "map< @[%a,@ %a@] >" pp a pp b);
    Metadata.pp out self.meta
end

module Field = struct
  type req = Default | Required | Optional

  type t = {
    id: int option;
    req: req;
    name: string;
    ty: Type.t;
    default: Const_value.t option;
  }

  let string_of_req = function
    | Default -> ""
    | Required -> "required"
    | Optional -> "optional"

  let pp out (f : t) =
    let pp_id out = function
      | None -> ()
      | Some i -> fpf out "(%d:) " i
    in
    let pp_default out = function
      | None -> ()
      | Some d -> fpf out " := %a" Const_value.pp d
    in
    fpf out "@[%a%s: %s %a%a;@]" pp_id f.id f.name (string_of_req f.req) Type.pp
      f.ty pp_default f.default
end

module Function_type = struct
  type t = Void | Ty of Type.t

  let pp out = function
    | Void -> fpf out "void"
    | Ty ty -> Type.pp out ty
end

module Function = struct
  type t = {
    oneway: bool;
    ty: Function_type.t;
    name: identifier;
    args: Field.t list;
    throws: Field.t list option;
  }

  let pp out (self : t) =
    let oneway =
      if self.oneway then
        "oneway "
      else
        ""
    in
    let pp_throw out = function
      | None -> ()
      | Some f -> fpf out "@ throws (@[%a@])" (pp_list_sp Field.pp) f
    in
    fpf out "@[<2>%s%a %s (@[%a@])%a@]" oneway Function_type.pp self.ty
      self.name (pp_list_sp Field.pp) self.args pp_throw self.throws
end

module Header = struct
  type namespace_scope = string

  type t =
    | Include of string
    | Cpp_include of string
    | Namespace of namespace_scope * identifier

  let pp out = function
    | Include s -> fpf out "include %S" s
    | Cpp_include s -> fpf out "cpp_include %S" s
    | Namespace (sc, i) -> fpf out "namespace (%S, %S)" sc i
end

module Definition = struct
  type enum_case = { e_name: identifier; e_num: int option }

  type view =
    | Const of { ty: Type.t; value: Const_value.t }
    | TypeDef of { ty: Type.t }
    | Enum of { cases: enum_case list }
    | Struct of { fields: Field.t list }
    | Union of { fields: Field.t list }
    | Exception of { fields: Field.t list }
    | Service of { extends: identifier option; funs: Function.t list }

  type t = { name: identifier; meta: Metadata.t; view: view }

  let pp_enum_case out (e : enum_case) =
    let pp_n out = function
      | None -> ()
      | Some n -> fpf out " = %d" n
    in
    fpf out "%s%a;" e.e_name pp_n e.e_num

  let pp out (self : t) =
    let pp_fields out l =
      fpf out "{@;<1 0>%a@;<1 -2>}" (pp_list_sp Field.pp) l
    in
    let name = self.name in
    (match self.view with
    | Const { ty; value } ->
      fpf out "@[const %s : %a :=@ %a@]" name Type.pp ty Const_value.pp value
    | TypeDef { ty } -> fpf out "@[typedef %s :=@ %a@]" name Type.pp ty
    | Enum { cases } ->
      fpf out "@[<hv2>enum %s {@;<1 0>%a@;<1 -2>}@]" name
        (pp_list_sp pp_enum_case) cases
    | Struct { fields } -> fpf out "@[<hv2>struct %s %a@]" name pp_fields fields
    | Union { fields } -> fpf out "@[<hv2>union %s %a@]" name pp_fields fields
    | Exception { fields } ->
      fpf out "@[<hv2>exception %s %a@]" name pp_fields fields
    | Service { extends; funs } ->
      let pp_extend out = function
        | None -> ()
        | Some e -> fpf out "@ extends %s" e
      in
      fpf out "@[<hv2>service %s%a {@;<1 0>%a@;<1 -2>}@]" name pp_extend extends
        (pp_list_sp Function.pp) funs);

    Metadata.pp out self.meta;
    fpf out ";"

  let show = Format.asprintf "%a" pp
end

module File = struct
  type t = { headers: Header.t list; defs: Definition.t list }

  let make headers defs : t = { headers; defs }

  let pp out self =
    fpf out "{@[ headers: [@[<hv>%a@]];@ defs: [@[<hv>%a@]]@ @]}"
      (pp_list_sp Header.pp) self.headers (pp_list_sp Definition.pp) self.defs
end

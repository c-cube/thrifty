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

module Field_type = struct
  type t =
    | Base of field_type
    | Named of identifier
    | List of t
    | Map of t * t
    | Set of t

  let rec pp out = function
    | Base n -> Fmt.string out (string_of_field_type n)
    | Named s -> Fmt.string out s
    | List l -> fpf out "list< @[%a@] >" pp l
    | Set l -> fpf out "set< @[%a@] >" pp l
    | Map (a, b) -> fpf out "map< @[%a,@ %a@] >" pp a pp b
end

module Field = struct
  type req = Default | Required | Optional

  type t = {
    id: int option;
    req: req;
    name: string;
    ty: Field_type.t;
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
    fpf out "@[%a%s: %s %a%a;@]" pp_id f.id f.name (string_of_req f.req)
      Field_type.pp f.ty pp_default f.default
end

module Struct_fields = struct
  type t = { fields: Field.t list }

  let pp out { fields } =
    fpf out "{@;<1 0>%a@;<1 -2>}" (pp_list_sp Field.pp) fields
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

  type t =
    | Const of { ty: Field_type.t; name: identifier; value: Const_value.t }
    | TypeDef of { ty: Field_type.t; name: identifier }
    | Enum of { name: identifier; cases: enum_case list }
    | Struct of { name: identifier; fields: Struct_fields.t }

  let pp_enum_case out (e : enum_case) =
    let pp_n out = function
      | None -> ()
      | Some n -> fpf out " = %d" n
    in
    fpf out "%s%a;" e.e_name pp_n e.e_num

  let pp out = function
    | Const { ty; name; value } ->
      fpf out "@[const %s : %a :=@ %a@];" name Field_type.pp ty Const_value.pp
        value
    | TypeDef { ty; name } ->
      fpf out "@[typedef %s :=@ %a@];" name Field_type.pp ty
    | Enum { name; cases } ->
      fpf out "@[<hv2>enum %s {@;<1 0>%a@;<1 -2>}@];" name
        (pp_list_sp pp_enum_case) cases
    | Struct { name; fields } ->
      fpf out "@[<hv2>struct %s %a@];" name Struct_fields.pp fields

  let show = Format.asprintf "%a" pp
end

module File = struct
  type t = { headers: Header.t list; defs: Definition.t list }

  let make headers defs : t = { headers; defs }

  let pp out self =
    fpf out "{@[ headers: [@[<hv>%a@]];@ defs: [@[<hv>%a@]]@ @]}"
      (pp_list_sp Header.pp) self.headers (pp_list_sp Definition.pp) self.defs
end

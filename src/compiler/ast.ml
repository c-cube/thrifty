module Fmt = CCFormat

module Const_value = struct
  type t =
    | Bool of bool
    | Int of int64
    | Double of float
    | String of string
    | List of t list
    | Map of (t * t) list

  let rec pp out = function
    | Bool b -> Fmt.pp_print_bool out b
    | Int i -> Fmt.fprintf out "%Ld" i
    | Double f -> Fmt.fprintf out "%f" f
    | String s -> Fmt.fprintf out "%S" s
    | List l -> Fmt.Dump.list pp out l
    | Map l ->
      let pp_pair out (k, v) = Fmt.fprintf out "@[%a: %a@]" pp k pp v in
      Fmt.fprintf out "{@[%a@]}" (Fmt.list pp_pair) l
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
    | List l -> Fmt.fprintf out "list< @[%a@] >" pp l
    | Set l -> Fmt.fprintf out "set< @[%a@] >" pp l
    | Map (a, b) -> Fmt.fprintf out "map< @[%a,@ %a@] >" pp a pp b
end

module Statement = struct
  type namespace_scope = string

  type t =
    | Include of string
    | Cpp_include of string
    | Namespace of namespace_scope * identifier
    | Const of { ty: Field_type.t; name: identifier; value: Const_value.t }
end

module Fmt = CCFormat

let fpf = Fmt.fprintf

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
    | Int i -> fpf out "%Ld" i
    | Double f -> fpf out "%f" f
    | String s -> fpf out "%S" s
    | List l -> Fmt.Dump.list pp out l
    | Map l ->
      let pp_pair out (k, v) = fpf out "@[%a: %a@]" pp k pp v in
      fpf out "{@[%a@]}" (Fmt.list pp_pair) l
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

(* TODO
   module Def_type = struct
     type field = { name: string; ty: Field_type.t; default: Const_value.t option }
     type t = Struct of field list | Union of field list

     let pp_field out (f : field) =
       let pp_default out = function
         | None -> ()
         | Some d -> fpf out " := %a" Const_value.pp d
       in
       fpf out "@[%s: %a%a;@]" f.name Field_type.pp f.ty pp_default f.default

     let pp out = function
       | Struct fields ->
         fpf out "{@[%a@]}" (Fmt.list ~sep:(return ()) pp_field) fields
       | Union fields ->
         fpf out "union {@[%a@]}" (Fmt.list ~sep:(const ()) pp_field) fields
   end
*)

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
  type t =
    | Const of { ty: Field_type.t; name: identifier; value: Const_value.t }
    | TypeDef of { ty: Field_type.t; name: identifier }

  let pp out = function
    | Const { ty; name; value } ->
      fpf out "@[const %s : %a :=@ %a@];" name Field_type.pp ty Const_value.pp
        value
    | TypeDef { ty; name } ->
      fpf out "@[typedef %s :=@ %a@];" name Field_type.pp ty

  let show = Format.asprintf "%a" pp
end

module File = struct
  type t = { headers: Header.t list; defs: Definition.t list }

  let make headers defs : t = { headers; defs }

  let pp out self =
    fpf out "{@[ headers: %a;@ defs: %a@ @]}" (Fmt.Dump.list Header.pp)
      self.headers
      (Fmt.Dump.list Definition.pp)
      self.defs
end

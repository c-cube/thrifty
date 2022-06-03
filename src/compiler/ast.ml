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

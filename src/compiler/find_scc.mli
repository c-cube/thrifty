open Ast

type t = { headers: Header.t list; def_scc_l: Definition.t list list }

val top : File.t -> t
(** Group definitions into clusters of mutually recursive definitions. *)

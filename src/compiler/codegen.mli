val parse_file : string -> Ast.File.t

module CG : sig
  type t

  val create : unit -> t
  val encode_file : t -> pp:bool -> Ast.File.t -> unit
  val code : t -> string
  val write_code : out_channel -> t -> unit
end

val codegen :
  to_stdout:bool ->
  out:string ->
  pp:bool ->
  filename:string ->
  Ast.File.t ->
  unit

(**/**)

val debug : bool ref

(**/**)

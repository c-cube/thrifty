open Thrifty_compiler_lib

let () =
  let file = ref "" in
  let cat = ref false in
  let out = ref "" in
  let pp = ref false in
  let stdout = ref false in
  let opts =
    [
      "--cat", Arg.Set cat, " print type definitions";
      "-d", Arg.Set Codegen.debug, " debug mode";
      "-o", Arg.Set_string out, " codegen: print code to given file";
      "--stdout", Arg.Set stdout, " codegen: print code to stdout";
      "--pp", Arg.Set pp, " codegen: generate pretty printer code";
    ]
    |> Arg.align
  in
  Arg.parse opts (fun f -> file := f) "usage: smol_thrift_compiler [opt]* file";
  if !file = "" then failwith "file needed";
  Format.printf "in=%S, out=%S@." !file !out;
  let file_ast = Codegen.parse_file !file in
  if !cat then Format.printf "%a@.@." Ast.File.pp file_ast;
  if !stdout || !out <> "" then
    Codegen.codegen ~to_stdout:!stdout ~pp:!pp ~out:!out ~filename:!file
      file_ast;
  ()

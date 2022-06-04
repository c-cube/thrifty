open Smol_thrift_compiler_lib

let () =
  let file = Sys.argv.(1) in
  Printf.printf "parse %S\n" file;

  let content = CCIO.File.read_exn file in
  let t_file = Parser.parse_string Parser.file content in
  match t_file with
  | Ok ast ->
    Printf.printf "parsed ok\n";
    Format.printf "content:@.";
    Format.printf "%a@." Ast.File.pp ast
  | Error err ->
    Printf.printf "error: %s\n%!" err;
    exit 1

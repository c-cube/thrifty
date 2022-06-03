module P = CCParse

type 'a t = 'a P.t

let parse_string = P.parse_string

open P.Infix
open P

let read_multiline_comment =
  chars_fold `normal ~f:(fun acc c ->
      match acc, c with
      | `normal, '*' -> `Continue `star
      | `normal, _ -> `Continue `normal
      | `star, '/' -> `Consume_and_stop `normal
      | `star, _ -> `Continue `normal)
  *> return ()

let skip_white =
  let rec loop () =
    skip_white
    *> try_or_l ~else_:(return ())
         [
           ( lookahead (string "//" *> return ()),
             suspend (fun () -> line *> loop ()) );
           ( lookahead (string "/*" *> return ()),
             suspend (fun () -> read_multiline_comment *> loop ()) );
         ]
  in
  loop ()

let is_letter_or_under = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_letter_or_digit_under = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> true
  | _ -> false

let identifier =
  skip_white
  *> let+ c = char_if is_letter_or_under
     and+ rest = take_if is_letter_or_digit_under in
     String.make 1 c ^ Slice.to_string rest

let exact_keyword s =
  let* s' = identifier in
  if s = s' then
    return ()
  else
    fail_lazy (fun () -> Printf.sprintf "expected %S" s)

let int_const_ =
  let* sign =
    char '+' *> return true <|> char '-' *> return false <|> return true
  in
  let+ c, _ =
    chars_fold 0 ~f:(fun acc c ->
        match c with
        | '0' .. '9' -> `Continue ((10 * acc) + Char.code c - Char.code '0')
        | _ when acc = 0 -> `Fail "expected an int"
        | _ -> `Stop acc)
  in
  Ast.Const_value.Int (Int64.of_int c)

let literal_ =
  skip_white
  *> (char '"'
      *> chars_fold () ~f:(fun () -> function
           | '"' -> `Stop ()
           | _ -> `Continue ())
     <* char '"' >|= snd >|= Slice.to_string
     <|> (char '\''
          *> chars_fold () ~f:(fun () -> function
               | '\'' -> `Stop ()
               | _ -> `Continue ())
         <* char '\'' >|= snd >|= Slice.to_string)
     <?> "expected literal")
  >|= fun s -> Ast.Const_value.String s

let bool_const_ =
  let+ b =
    string "true" *> return true
    <|> string "false" *> return false
    <?> "expected bool"
  in
  Ast.Const_value.Bool b

let optional_ p = p <|> return ()

let list_sep_ =
  skip_white
  *> char_if ~descr:"list separator" (function
       | ',' | ';' -> true
       | _ -> false)
  *> return ()

let map_const_ self =
  let rec pairs acc =
    skip_white
    *> try_or (char '}')
         ~f:(fun _ -> return (Ast.Const_value.Map (List.rev acc)))
         ~else_:
           (let* k = self
            and* _ = skip_white *> char ':'
            and* v = self
            and* _ = optional_ list_sep_ in
            pairs ((k, v) :: acc))
  in
  char '{' *> pairs []

let list_const_ self =
  let rec loop acc =
    skip_white
    *> try_or (char ']')
         ~f:(fun _ -> return (Ast.Const_value.List (List.rev acc)))
         ~else_:
           (let* x = self and* _ = optional_ list_sep_ in
            loop (x :: acc))
  in
  char '[' *> loop []

(* TODO: double constant *)

let const_value : Ast.Const_value.t P.t =
  fix @@ fun self ->
  skip_white
  *> try_or_l ~msg:"expected constant value"
       [
         ( lookahead_ignore
             (char_if (function
               | '-' | '+' | '0' .. '9' -> true
               | _ -> false)),
           int_const_ );
         ( lookahead_ignore
             (char_if (function
               | '"' | '\'' -> true
               | _ -> false)),
           literal_ );
         lookahead_ignore bool_const_, bool_const_;
         lookahead_ignore (char '{'), map_const_ self;
         lookahead_ignore (char '['), list_const_ self;
       ]

let base_type =
  exact_keyword "bool" *> return T_BOOL
  <|> exact_keyword "byte" *> return T_BYTE
  <|> exact_keyword "i8" *> return T_I8
  <|> exact_keyword "i16" *> return T_I16
  <|> exact_keyword "i32" *> return T_I32
  <|> exact_keyword "i64" *> return T_I64
  <|> exact_keyword "double" *> return T_DOUBLE
  <|> exact_keyword "string" *> return T_STRING
  <|> exact_keyword "binary" *> return T_BINARY
  <?> "base type"

let cpp_type_ = skip_white *> exact_keyword "cpp_type" *> literal_ *> return ()

let container_type self =
  (exact_keyword "list" *> skip_white *> char '<'
  *> let+ e = self <* skip_white <* char '>' <* optional_ cpp_type_ in
     Ast.Field_type.List e)
  <|> (exact_keyword "set" *> optional_ cpp_type_ *> skip_white *> char '<'
      *> let+ e = self <* skip_white <* char '>' in
         Ast.Field_type.Set e)
  <|> (exact_keyword "map" *> optional_ cpp_type_ *> skip_white *> char '<'
      *> let+ k = self
         and+ _ = skip_white *> char ','
         and+ v = self
         and+ _ = skip_white <* char '>' in
         Ast.Field_type.Map (k, v))
  <?> "expected container type"

let field_type =
  fix @@ fun self ->
  (skip_white
  *> let+ s = base_type in
     Ast.Field_type.Base s)
  <|> container_type self
  <|> (let* s = identifier in
       if s = "list" || s = "map" || s = "set" then
         fail "bad container"
       else
         return @@ Ast.Field_type.Named s)
  <?> "expected field type"

let statement : Ast.Statement.t P.t =
  skip_white
  *> try_or_l ~msg:"expected statement"
       [
         ( exact_keyword "const" *> return (),
           exact_keyword "const"
           *> let+ ty = field_type
              and+ name = identifier
              and+ _ = skip_white *> char '='
              and+ v = const_value
              and+ _ = optional_ list_sep_ in
              Ast.Statement.Const { ty; name; value = v } );
       ]

let file =
  let rec loop acc =
    skip_white
    *> try_or eoi
         ~f:(fun _ -> return @@ List.rev acc)
         ~else_:
           (let* st = statement in
            loop (st :: acc))
  in
  loop []

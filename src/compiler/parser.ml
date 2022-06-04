module P = CCParse

type 'a t = 'a P.t

let parse_string = P.parse_string

open P

let _show_opt_ pp = function
  | None -> "None"
  | Some x -> Format.asprintf "Some `@[%a@]`" pp x

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
           ( lookahead (string "#" *> return ()),
             suspend (fun () -> line *> loop ()) );
           ( lookahead (string "/*" *> return ()),
             suspend (fun () -> read_multiline_comment *> loop ()) );
         ]
  in
  loop ()

let is_letter_or_under = function
  | 'A' .. 'Z' | 'a' .. 'z' | '_' -> true
  | _ -> false

let is_letter_or_digit_under_dot = function
  | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '.' -> true
  | _ -> false

let identifier =
  skip_white
  *> let+ c = char_if is_letter_or_under
     and+ rest = take_if is_letter_or_digit_under_dot in
     String.make 1 c ^ Slice.to_string rest

let namespace_id =
  skip_white
  *> (char '*' *> return "*"
     <|> let+ c = char_if is_letter_or_under
         and+ rest = take_if (fun c -> not @@ is_white c) in
         String.make 1 c ^ Slice.to_string rest)

let exact_keyword s =
  let* s' = identifier in
  if s = s' then
    return ()
  else
    fail_lazy (fun () -> Printf.sprintf "expected %S" s)

let int_const_ =
  let* sign = char '+' *> return 1 <|> char '-' *> return (-1) <|> return 1 in
  let+ (_, c), _ =
    chars_fold (false, 0) ~f:(fun (parsed_any, acc) c ->
        match c with
        | '0' .. '9' ->
          `Continue (true, (10 * acc) + Char.code c - Char.code '0')
        | _ when not parsed_any -> `Fail "expected an int"
        | _ -> `Stop (true, acc))
  in
  sign * c

let double_const_ =
  let* sign =
    char '+' *> return 1. <|> char '-' *> return (-1.) <|> return 1.
  in
  let* st, s =
    chars_fold `mantissa ~f:(fun state c ->
        match state, c with
        | `mantissa, ('.' | '0' .. '9') -> `Continue `mantissa
        | `exp, ('+' | '-' | '0' .. '9') -> `Continue `exp
        | `mantissa, ('e' | 'E') -> `Continue `exp
        | _ -> `Stop state)
  in
  let s = Slice.to_string s in
  if String.contains s '.' || st = `exp then (
    try return @@ (sign *. float_of_string s)
    with _ -> fail "expected double const"
  ) else
    fail "expected double const"

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

let equal = skip_white *> char '='
let in_braces p = skip_white *> char '{' *> p <* skip_white <* char '}'
let in_parens p = skip_white *> char '(' *> p <* skip_white <* char ')'

(* parse (k1='v1', k2='v2') *)
let metadata =
  skip_white
  *> try_or (char '(')
       ~f:(fun _ ->
         sep1
           ~by:(skip_white *> char ',')
           (let+ a = identifier and+ _ = equal and+ b = literal_ in
            a, b)
         <* skip_white <* char ')')
       ~else_:(return [])

let const_value : Ast.Const_value.t P.t =
  fix @@ fun self ->
  skip_white
  *> try_or_l ~msg:"expected constant value"
       [
         ( lookahead_ignore
             (char_if (function
               | '-' | '+' | '0' .. '9' -> true
               | _ -> false)),
           (let+ d = double_const_ in
            Ast.Const_value.Double d)
           <|> let+ n = int_const_ in
               Ast.Const_value.Int (Int64.of_int n) );
         ( lookahead_ignore
             (char_if (function
               | '"' | '\'' -> true
               | _ -> false)),
           let+ s = literal_ in
           Ast.Const_value.String s );
         lookahead_ignore bool_const_, bool_const_;
         lookahead_ignore (char '{'), map_const_ self;
         lookahead_ignore (char '['), list_const_ self;
         ( lookahead_ignore identifier,
           let+ n = identifier in
           Ast.Const_value.Named n );
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
  let+ view =
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
  and+ meta = metadata in
  Ast.Field_type.{ view; meta }

let field_type =
  fix @@ fun self ->
  (skip_white
  *> let+ s = base_type and+ meta = metadata in
     Ast.Field_type.{ view = Base s; meta })
  <|> container_type self
  <|> (let* s = identifier and* meta = metadata in
       if s = "list" || s = "map" || s = "set" then
         fail "bad container"
       else
         return @@ Ast.Field_type.{ view = Named s; meta })
  <?> "expected type"

let header : Ast.Header.t option P.t =
  skip_white
  *> try_or_l ~msg:"expected header"
       [
         ( exact_keyword "include" *> return (),
           exact_keyword "include"
           *> let+ s = literal_ in
              Some (Ast.Header.Include s) );
         ( exact_keyword "cpp_include" *> return (),
           exact_keyword "cpp_include"
           *> let+ s = literal_ in
              Some (Ast.Header.Cpp_include s) );
         ( exact_keyword "namespace" *> return (),
           exact_keyword "namespace"
           *> let+ sc = namespace_id
              and+ ns = namespace_id
              and+ _m = metadata
              and+ _ = optional_ list_sep_ in
              Some (Ast.Header.Namespace (sc, ns)) );
         eoi, return None;
       ]
       ~else_:(return None)

let list_until_ p ~until:close =
  let rec loop acc =
    skip_white
    *> try_or (lookahead_ignore close)
         ~f:(fun _ -> return @@ List.rev acc)
         ~else_:
           ( suspend @@ fun () ->
             let* x = p in
             loop (x :: acc) )
  in
  loop []

let enum_cases =
  let case =
    let+ e_name = identifier
    and+ e_num =
      skip_white
      *> ((let+ _ = equal and+ n = skip_white *> int_const_ in
           Some n)
         <|> return None)
    and+ _ = optional_ list_sep_ in
    { Ast.Definition.e_num; e_name }
  in
  list_until_ ~until:(char '}') case

let field_id =
  (let+ n = int_const_ <* char ':' in
   Some n)
  <|> return None

let field_req =
  skip_white
  *> (exact_keyword "required" *> return Ast.Field.Required
     <|> exact_keyword "optional" *> return Ast.Field.Optional
     <|> return Ast.Field.Default)

let field =
  let+ id = field_id
  and+ req = field_req
  and+ ty = field_type
  and+ name = identifier
  and+ default =
    (let+ v = equal *> const_value in
     Some v)
    <|> return None
  and+ _ = metadata
  and+ _ = optional_ list_sep_ in
  { Ast.Field.id; req; ty; name; default }

let field_list = list_until_ ~until:(char '}') field

let service_functions =
  let fun_ =
    let+ oneway = exact_keyword "oneway" *> return true <|> return false
    and+ ty =
      exact_keyword "void" *> return Ast.Function_type.Void
      <|> let+ ty = field_type in
          Ast.Function_type.Ty ty
    and+ name = identifier
    and+ args = in_parens (list_until_ ~until:(char ')') field)
    and+ _ = skip_white
    and+ throws =
      try_or (exact_keyword "throws")
        ~f:(fun _ ->
          let+ l = in_parens (list_until_ ~until:(char ')') field) in
          Some l)
        ~else_:(return None)
    and+ _ = metadata
    and+ _ = optional_ list_sep_ in
    { Ast.Function.oneway; ty; name; args; throws }
  in
  list_until_ ~until:(char '}') fun_

let def : Ast.Definition.t option P.t =
  (*Debug_.trace_success_or_fail "def" ~print:(_show_opt_ Ast.Definition.pp)
    @@*)
  skip_white
  *> try_or_l ~msg:"expected definition"
       [
         ( exact_keyword "const" *> return (),
           exact_keyword "const"
           *> let+ ty = field_type
              and+ name = identifier
              and+ _ = equal
              and+ v = const_value
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some Ast.Definition.{ meta; view = Const { ty; name; value = v } }
         );
         ( exact_keyword "typedef" *> return (),
           exact_keyword "typedef"
           *> let+ ty = field_type
              and+ name = identifier
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some Ast.Definition.{ meta; view = TypeDef { ty; name } } );
         ( exact_keyword "struct" *> return (),
           exact_keyword "struct"
           *> let+ name = identifier
              and+ _ = optional_ (skip_white *> exact_keyword "xsd_all")
              and+ fields = in_braces field_list
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some Ast.Definition.{ meta; view = Struct { name; fields } } );
         ( exact_keyword "union" *> return (),
           exact_keyword "union"
           *> let+ name = identifier
              and+ _ = optional_ (skip_white *> exact_keyword "xsd_all")
              and+ fields = in_braces field_list
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some Ast.Definition.{ meta; view = Union { name; fields } } );
         ( exact_keyword "exception" *> return (),
           exact_keyword "exception"
           *> let+ name = identifier
              and+ _ = optional_ (skip_white *> exact_keyword "xsd_all")
              and+ fields = in_braces field_list
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some Ast.Definition.{ meta; view = Exception { name; fields } } );
         ( exact_keyword "enum" *> return (),
           exact_keyword "enum"
           *> let+ name = identifier
              and+ cases = in_braces enum_cases
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some Ast.Definition.{ meta; view = Enum { name; cases } } );
         ( exact_keyword "service" *> return (),
           exact_keyword "service"
           *> let+ name = identifier
              and+ extends =
                skip_white
                *> ((exact_keyword "extends"
                    *> let+ s = identifier in
                       Some s)
                   <|> return None)
              and+ funs = in_braces service_functions
              and+ meta = metadata
              and+ _ = optional_ list_sep_ in
              Some
                Ast.Definition.{ meta; view = Service { name; extends; funs } }
         );
         eoi, return None;
       ]

let file =
  let rec body headers acc =
    skip_white
    *> try_or eoi
         ~f:(fun _ -> return @@ Ast.File.make headers (List.rev acc))
         ~else_:
           (let* d = def in
            match d with
            | Some d -> body headers (d :: acc)
            | None -> return @@ Ast.File.make headers (List.rev acc))
  in
  let rec headers acc =
    skip_white
    *> try_or eoi
         ~f:(fun _ -> return @@ Ast.File.make (List.rev acc) [])
         ~else_:
           (let* h = header in
            match h with
            | Some h -> headers (h :: acc)
            | None -> body (List.rev acc) [])
  in
  headers []

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
  char_if ~descr:"list separator" (function
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

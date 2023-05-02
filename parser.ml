open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) =
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) =
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

let rec parse_expr toks =
    parse_let toks

and parse_let toks =
    match lookahead toks with
    | Some Tok_Let ->
        let toks' = match_token toks Tok_Let in
        (match lookahead toks' with
        | Some Tok_Rec ->
            (match lookahead_many toks' 1 with
            | Some Tok_ID (x) ->
                let toks' = match_many toks' [Tok_Rec; Tok_ID(x); Tok_Equal] in
                let (toks', e1) = parse_expr toks' in
                let toks' = match_token toks' Tok_In in
                let (toks', e2) = parse_expr toks' in
                (toks', Let (x, true, e1, e2))
            | _ -> raise (InvalidInputException "expected Tok_ID"))
        | Some Tok_ID (x) ->
            let toks' = match_many toks' [Tok_ID(x); Tok_Equal] in
            let (toks', e1) = parse_expr toks' in
            let toks' = match_token toks' Tok_In in
            let (toks', e2) = parse_expr toks' in
            (toks', Let (x, false, e1, e2))
        | _ -> raise (InvalidInputException "expected Tok_ID or Tok_Rec"))
    | _ -> parse_function toks

and parse_function toks =
    match lookahead toks with
    | Some Tok_Fun ->
        let toks' = match_token toks Tok_Fun in
        (match lookahead toks' with
        | Some Tok_ID x ->
            let toks' = match_many toks' [Tok_ID (x); Tok_Arrow] in
            let (toks', e1) = parse_expr toks' in
            (toks', Fun (x, e1))
        | _ -> raise (InvalidInputException "expected Tok_ID"))
    | _ -> parse_if toks

and parse_if toks =
    match lookahead toks with
    | Some Tok_If ->
        let toks' = match_token toks Tok_If in
        let (toks', e1) = parse_expr toks' in
        let toks' = match_token toks' Tok_Then in
        let (toks', e2) = parse_expr toks' in
        let toks' = match_token toks' Tok_Else in
        let (toks', e3) = parse_expr toks' in
        (toks', If (e1, e2, e3))
    | _ -> parse_or toks

and parse_or toks =
    let (toks', e1) = parse_and toks in
    match lookahead toks' with
    | Some Tok_Or ->
        let toks' = match_token toks' Tok_Or in
        let (toks', e2) = parse_or toks' in
        (toks', Binop (Or, e1, e2))
    | _ -> (toks', e1)

and parse_and toks =
    let (toks', e1) = parse_equality toks in
    match lookahead toks' with
    | Some Tok_And ->
        let toks' = match_token toks' Tok_And in
        let (toks', e2) = parse_and toks' in
        (toks', Binop (And, e1, e2))
    | _ -> (toks', e1)

and parse_equality toks =
    let (toks', e1) = parse_relation toks in
    match lookahead toks' with
    | Some Tok_Equal ->
        let toks' = match_token toks' Tok_Equal in
        let (toks', e2) = parse_equality toks' in
        (toks', Binop (Equal, e1, e2))
    | Some Tok_NotEqual ->
        let toks' = match_token toks' Tok_NotEqual in
        let (toks', e2) = parse_equality toks' in
        (toks', Binop (NotEqual, e1, e2))
    | _ -> (toks', e1)

and parse_relation toks =
    let (toks', e1) = parse_add toks in
    match lookahead toks' with
    | Some Tok_Greater ->
        let toks' = match_token toks' Tok_Greater in
        let (toks', e2) = parse_relation toks' in
        (toks', Binop (Greater, e1, e2))
    | Some Tok_Less ->
        let toks' = match_token toks' Tok_Less in
        let (toks', e2) = parse_relation toks' in
        (toks', Binop (Less, e1, e2))
    | Some Tok_GreaterEqual ->
        let toks' = match_token toks' Tok_GreaterEqual in
        let (toks', e2) = parse_relation toks' in
        (toks', Binop (GreaterEqual, e1, e2))
    | Some Tok_LessEqual ->
        let toks' = match_token toks' Tok_LessEqual in
        let (toks', e2) = parse_relation toks' in
        (toks', Binop (LessEqual, e1, e2))
    | _ -> (toks', e1)

and parse_add toks =
    let (toks', e1) = parse_multiply toks in
    match lookahead toks' with
    | Some Tok_Add ->
        let toks' = match_token toks' Tok_Add in
        let (toks', e2) = parse_add toks' in
        (toks', Binop (Add, e1, e2))
    | Some Tok_Sub ->
        let toks' = match_token toks' Tok_Sub in
        let (toks', e2) = parse_add toks' in
        (toks', Binop (Sub, e1, e2))
    | _ -> (toks', e1)

and parse_multiply toks =
    let (toks', e1) = parse_concat toks in
    match lookahead toks' with
    | Some Tok_Mult ->
        let toks' = match_token toks' Tok_Mult in
        let (toks', e2) = parse_multiply toks' in
        (toks', Binop (Mult, e1, e2))
    | Some Tok_Div ->
        let toks' = match_token toks' Tok_Div in
        let (toks', e2) = parse_multiply toks' in
        (toks', Binop (Div, e1, e2))
    | _ -> (toks', e1)

and parse_concat toks =
    let (toks', e1) = parse_unary toks in
    match lookahead toks' with
    | Some Tok_Concat ->
        let toks' = match_token toks' Tok_Concat in
        let (toks', e2) = parse_concat toks' in
        (toks', Binop (Concat, e1, e2))
    | _ -> (toks', e1)

and parse_unary toks =
    match lookahead toks with
    | Some Tok_Not ->
        let toks' = match_token toks Tok_Not in
        let (toks'', e1) = parse_unary toks' in
        (toks'', Not (e1))
    | _ -> parse_function_call toks

and parse_function_call toks =
    let (toks', e1) = parse_primary toks in
    match lookahead toks' with
    | Some Tok_Int x ->
        let (toks', e2) = parse_primary toks' in
        (toks', FunctionCall (e1, e2))
    | Some Tok_Bool x ->
        let (toks', e2) = parse_primary toks' in
        (toks', FunctionCall (e1, e2))
    | Some Tok_String x ->
        let (toks', e2) = parse_primary toks' in
        (toks', FunctionCall (e1, e2))
    | Some Tok_ID x ->
        let (toks', e2) = parse_primary toks' in
        (toks', FunctionCall (e1, e2))
    | Some Tok_LParen ->
        let toks' = match_token toks' Tok_LParen in
        let (toks', e2) = parse_expr toks' in
        let toks' = match_token toks' Tok_RParen in
        (toks', FunctionCall (e1, e2))
    | _ -> (toks', e1)

and parse_primary toks =
    match lookahead toks with
    | Some Tok_Int (x) -> (match_token toks (Tok_Int (x)), Value (Int (x)))
    | Some Tok_Bool (x) -> (match_token toks (Tok_Bool (x)), Value (Bool (x)))
    | Some Tok_String (x) -> (match_token toks (Tok_String (x)), Value (String (x)))
    | Some Tok_ID (x) -> (match_token toks (Tok_ID (x)), ID (x))
    | _ ->
        let toks' = match_token toks Tok_LParen in
        let (toks', e1) = parse_expr toks' in
        let toks' = match_token toks' Tok_RParen in
        (toks', e1)


(* Part 3: Parsing mutop *)

let rec parse_mutop toks =
    parse_noop toks

and parse_noop toks =
    match lookahead toks with
    | Some Tok_DoubleSemi ->
        let toks' = match_token toks Tok_DoubleSemi in
        (toks', NoOp)
    | _ -> parse_def toks

and parse_def toks =
    match lookahead toks with
    | Some Tok_Def ->
        let toks' = match_token toks Tok_Def in
        (match lookahead toks' with
        | Some Tok_ID x ->
            let toks' = match_many toks' [Tok_ID x; Tok_Equal] in
            let (toks', e1) = parse_expr toks' in
            let toks' = match_token toks' Tok_DoubleSemi in
            (toks', Def (x, e1))
        | _ -> raise (InvalidInputException "expected Tok_ID"))
    | _ -> parse_expr_mutop toks

and parse_expr_mutop toks =
    let (toks', e1) = parse_expr toks in
    match lookahead toks' with
    | Some Tok_DoubleSemi ->
        let toks' = match_token toks' Tok_DoubleSemi in
        (toks', Expr (e1))
    | _ -> parse_noop toks



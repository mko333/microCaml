open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let rec tokenize input =
    (* get rid of leading and trailing whitespace in input *)
    let s = String.trim input in
    match s with
    | "" -> []
    | str ->
        (* literals (minus Tok_ID) *)
        if Str.string_match (Str.regexp {|^true|}) str 0 then
            (Tok_Bool true) :: (tokenize (Str.replace_first (Str.regexp {|^true|}) "" str))
        else if Str.string_match (Str.regexp {|^false|}) str 0 then
            (Tok_Bool false) :: (tokenize (Str.replace_first (Str.regexp {|^false|}) "" str))
        else if Str.string_match (Str.regexp {|^[0-9]+|}) str 0 then
            let tok = Str.matched_group 0 str in
            (Tok_Int (int_of_string tok)) :: (tokenize (Str.replace_first (Str.regexp {|^[0-9]+|}) "" str))
        else if Str.string_match (Str.regexp {|^(-[0-9]+)|}) str 0 then
            let tok = Str.matched_group 0 str in
            let num = int_of_string (Str.global_replace (Str.regexp {|[()]|}) "" tok) in
            (Tok_Int num) :: (tokenize (Str.replace_first (Str.regexp {|^(-[0-9]+)|}) "" str))
        else if Str.string_match (Str.regexp {|^\"[^\"]*\"|}) str 0 then
            let tok = Str.matched_group 0 str in
            let txt = (Str.global_replace (Str.regexp {|[\"]|}) "" tok) in
            (Tok_String txt) :: (tokenize (Str.replace_first (Str.regexp {|^\"[^\"]*\"|}) "" str))

        (* keywords (and Tok_ID) *)
        else if Str.string_match (Str.regexp {|^\(not \)\|\(not\)$|}) str 0 then
            Tok_Not :: (tokenize (Str.replace_first (Str.regexp {|^not|}) "" str))
        else if Str.string_match (Str.regexp {|^\(if \)\|\(if\)$|}) str 0 then
            Tok_If :: (tokenize (Str.replace_first (Str.regexp {|^if|}) "" str))
        else if Str.string_match (Str.regexp {|^\(then \)\|\(then\)$|}) str 0 then
            Tok_Then :: (tokenize (Str.replace_first (Str.regexp {|^then|}) "" str))
        else if Str.string_match (Str.regexp {|^\(else \)\|\(else\)|}) str 0 then
            Tok_Else :: (tokenize (Str.replace_first (Str.regexp {|^else|}) "" str))
        else if Str.string_match (Str.regexp {|^\(let \)\|\(let\)$|}) str 0 then
            Tok_Let :: (tokenize (Str.replace_first (Str.regexp {|^let|}) "" str))
        else if Str.string_match (Str.regexp {|^\(def \)\|\(def\)$|}) str 0 then
            Tok_Def :: (tokenize (Str.replace_first (Str.regexp {|^def|}) "" str))
        else if Str.string_match (Str.regexp {|^\(in \)\|\(in\)$|}) str 0 then
            Tok_In :: (tokenize (Str.replace_first (Str.regexp {|^in|}) "" str))
        else if Str.string_match (Str.regexp {|^\(rec \)\|\(rec\)$|}) str 0 then
            Tok_Rec :: (tokenize (Str.replace_first (Str.regexp {|^rec|}) "" str))
        else if Str.string_match (Str.regexp {|^\(fun \)\|\(fun\)$|}) str 0 then
            Tok_Fun :: (tokenize (Str.replace_first (Str.regexp {|^fun|}) "" str))
        else if Str.string_match (Str.regexp {|^[a-zA-Z][a-zA-Z0-9]*|}) str 0 then
            let tok = Str.matched_group 0 str in
            (Tok_ID tok) :: (tokenize (Str.replace_first (Str.regexp {|^[a-zA-Z][a-zA-Z0-9]*|}) "" str))

        (* other syntax *)
        else if Str.string_match (Str.regexp {|^(|}) str 0 then
            Tok_LParen :: (tokenize (Str.replace_first (Str.regexp {|^(|}) "" str))
        else if Str.string_match (Str.regexp {|^)|}) str 0 then
            Tok_RParen :: (tokenize (Str.replace_first (Str.regexp {|^)|}) "" str))
        else if Str.string_match (Str.regexp {|^=|}) str 0 then
            Tok_Equal :: (tokenize (Str.replace_first (Str.regexp {|^=|}) "" str))
        else if Str.string_match (Str.regexp {|^<>|}) str 0 then
            Tok_NotEqual :: (tokenize (Str.replace_first (Str.regexp {|^<>|}) "" str))
        else if Str.string_match (Str.regexp {|^>=|}) str 0 then
            Tok_GreaterEqual :: (tokenize (Str.replace_first (Str.regexp {|^>=|}) "" str))
        else if Str.string_match (Str.regexp {|^<=|}) str 0 then
            Tok_LessEqual :: (tokenize (Str.replace_first (Str.regexp {|^<=|}) "" str))
        else if Str.string_match (Str.regexp {|^>|}) str 0 then
            Tok_Greater :: (tokenize (Str.replace_first (Str.regexp {|^>|}) "" str))
        else if Str.string_match (Str.regexp {|^<|}) str 0 then
            Tok_Less :: (tokenize (Str.replace_first (Str.regexp {|^<|}) "" str))
        else if Str.string_match (Str.regexp {|^|||}) str 0 then
            Tok_Or :: (tokenize (Str.replace_first (Str.regexp {|^|||}) "" str))
        else if Str.string_match (Str.regexp {|^&&|}) str 0 then
            Tok_And :: (tokenize (Str.replace_first (Str.regexp {|^&&|}) "" str))
        else if Str.string_match (Str.regexp {|^->|}) str 0 then
            Tok_Arrow :: (tokenize (Str.replace_first (Str.regexp {|^->|}) "" str))
        else if Str.string_match (Str.regexp {|^\+|}) str 0 then
            Tok_Add :: (tokenize (Str.replace_first (Str.regexp {|^\+|}) "" str))
        else if Str.string_match (Str.regexp {|^-|}) str 0 then
            Tok_Sub :: (tokenize (Str.replace_first (Str.regexp {|^-|}) "" str))
        else if Str.string_match (Str.regexp {|^\*|}) str 0 then
            Tok_Mult :: (tokenize (Str.replace_first (Str.regexp {|^\*|}) "" str))
        else if Str.string_match (Str.regexp {|^/|}) str 0 then
            Tok_Div :: (tokenize (Str.replace_first (Str.regexp {|^/|}) "" str))
        else if Str.string_match (Str.regexp {|^\^|}) str 0 then
            Tok_Concat :: (tokenize (Str.replace_first (Str.regexp {|^\^|}) "" str))
        else if Str.string_match (Str.regexp {|^;;|}) str 0 then
            Tok_DoubleSemi :: (tokenize (Str.replace_first (Str.regexp {|^;;|}) "" str))

        (* fail *)
        else
            raise (InvalidInputException str);;

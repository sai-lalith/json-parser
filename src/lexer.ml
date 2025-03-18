open Stdlib
open Constants

let json_whitespace = [' '; '\t'; '\b'; '\n'; '\r']
let json_syntax = [json_comma; json_colon; json_leftbracket; json_rightbracket; json_leftbrace; json_rightbrace]

exception Not_Implemented
exception Unexpected_Character of char
exception Out_of_Bounds of string

let rest_of_string string = (String.sub string 1 (String.length string - 1))
let rec lex_string_aux string json_string = 
    if (String.length string = 0) then raise (Out_of_Bounds "Expected end-of-string quote")
    else
    if string.[0] = json_quote then
        (Some json_string, (rest_of_string string))
    else
        lex_string_aux (rest_of_string string) (json_string ^  Char.escaped  string.[0])
let lex_string string = 
    if string.[0] = json_quote then
        lex_string_aux  (rest_of_string string)  ""
    else
        (None, string)

let number_space = List.init 10 (fun x -> char_of_int x) @ ['.'; '-'; 'e']
let lex_number_aux string number_string =
    if List.exists (fun x -> x = string.[0]) number_space
        then lex_string_aux (rest_of_string string) (number_string ^ Char.escaped string.[0])
    else 
        
let lex_number string = 
    lex_number_aux string ""

    
let lex_boolean = raise Not_Implemented
let lex_null = raise Not_Implemented

(*Helper function on lex for pattern matching strings

let explode s =
    let rec expl i l =
      if i < 0 then l else
      expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) [];;
*)
  
let rec lex string tokens = 
    if String.length string = 0 then List.rev tokens
    else
        let (json_string, remaining) = lex_string string in
        if(json_string <> None) then lex remaining (Option.get json_string :: tokens)
        else
        let (json_string, remaining) = lex_number  in 
        if(json_string <> []) then lex remaining (json_string :: tokens)
        else
        let (json_string, remaining) = lex_boolean  in 
        if(json_string <> []) then lex remaining (json_string :: tokens)
        else
        let (json_string, remaining) = lex_null  in 
        if(json_string <> []) then lex remaining (json_string :: tokens)
        else
            if List.exists (fun x -> x = string.[0]) json_whitespace then 
            lex (String.sub string 1 (String.length string - 1)) tokens
            else
                if List.exists (fun x -> x = string.[0]) json_syntax then 
                    lex (String.sub string 1 (String.length string - 1) ) (Char.escaped string.[0] :: tokens)
        else 
            raise(Unexpected_Character string.[0])
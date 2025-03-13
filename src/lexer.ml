open Constants

let json_whitespace = [' '; '\t'; '\b'; '\n'; '\r']
let json_syntax = [json_comma; json_colon; json_leftbracket; json_rightbracket; json_leftbrace; json_rightbrace]

exception Not_Implemented
exception Unexpected_Character of char
let lex_string = raise Not_Implemented
let lex_number = raise Not_Implemented
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
        if(json_string = []) then lex remaining (json_string :: tokens)
        else
        let (json_string, remaining) = lex_number string in 
        if(json_string = []) then lex remaining (json_string :: tokens)
        else
        let (json_string, remaining) = lex_boolean string in 
        if(json_string = []) then lex remaining (json_string :: tokens)
        else
        let (json_string, remaining) = lex_null string in 
        if(json_string = []) then lex remaining (json_string :: tokens)
        else
            if List.exists (fun x -> x = string.[0]) json_whitespace then 
            lex (String.sub string 1 (String.length string)) tokens
            else
                if List.exists (fun x -> x = string.[0]) json_syntax then 
                    lex (String.sub string 1 (String.length string)) (string.[0] :: tokens)
        else 
            (*Make the error point to the character*)
            raise(Unexpected_Character string.[0])
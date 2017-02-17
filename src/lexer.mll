{
    open Lexing
    open Parser

    exception Lexing_error of string

    let keywords = [
        ("access", ACCESS);
        ("and", AND);
        ("begin", BEGIN);
        ("else", ELSE);
        ("elsif", ELSIF);
        ("end", END);
        ("false", FALSE);
        ("for", FOR);
        ("function", FUNCTION);
        ("if", IF);
        ("in", IN);
        ("is", IS);
        ("loop", LOOP);
        ("new", NEW);
        ("not", NOT);
        ("null", NULL);
        ("or", OR);
        ("out", OUT);
        ("procedure", PROCEDURE);
        ("record", RECORD);
        ("rem", REM);
        ("return", RETURN);
        ("reverse", REVERSE);
        ("then", THEN);
        ("true", TRUE);
        ("type", TYPE);
        ("use", USE);
        ("while", WHILE);
        ("with", WITH)
    ]
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = alpha (digit | alpha | "_")*

rule token = parse
    | eof { EOF }
    | [' ' '\t'] { token lexbuf }
    | "\n" { Lexing.new_line lexbuf; token lexbuf }
    | "--" { comment lexbuf }
    | digit+ as i { let i = int_of_string i in if i < 0 || i > 2147483648 then (raise (Lexing_error "integer overflow")) else CINT(i) }
    | "'" _ "'" as a { CCHAR(a.[1]) }
    | ident as s { let s = String.lowercase s in try List.assoc s keywords with | Not_found -> IDENT(s) }
    | ";" { SCOLON }
    | "." { DOT }
    | ".." { DDOT }
    | ":" { COLON }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | ":=" { CEQUAL }
    | "'" { APOS }
    | "," { COMMA }
    | "=" { EQUAL }
    | "/=" { NEQUAL }
    | "<" { LESS }
    | "<=" { LEQ }
    | ">" { GREATER }
    | ">=" { GEQ }
    | "+" { PLUS }
    | "-" { MINUS }
    | "*" { MUL }
    | "/" { DIV }
    | _ as s { raise (Lexing_error (Printf.sprintf "%C" s)) }

and comment = parse
    | eof { EOF }
    | "\n" { Lexing.new_line lexbuf; token lexbuf }
    | _ { comment lexbuf }
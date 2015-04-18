{
open Core.Std
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

let keywords = [("forall", FORALL); ("fun", FUN)]

let find_reserved s =
  match List.Assoc.find keywords s with
  | Some cstr -> cstr
  | None      -> ID s

}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "->"     { ARROW }
  | "*"      { ASTERISK }
  | "::"     { COLONCOLON }
  | "."      { PERIOD }
  | "("      { LPAREN }
  | ")"      { RPAREN }
  | id       { find_reserved (lexeme lexbuf) }
  | eof      { EOF }

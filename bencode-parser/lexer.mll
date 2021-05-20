{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let string = ['0'-'9']+ ':'
let int = 'i' '-'? ['0'-'9']+ 'e'
let newline = '\r' | '\n' | "\r\n"
let whitespace = [' ' '\t']+

rule read =
  parse
  | string {
    let str = lexeme lexbuf in
    let str_length = int_of_string (String.sub str 0 (String.length str - 1)) in
    STRING (
        if str_length = 0 then ""
        else read_string (Buffer.create str_length) (str_length - 1) lexbuf
    )
  }
  | int {
    let str = lexeme lexbuf in
    INT (Int64.of_string (String.sub str 1 (String.length str - 2)))
  }
  | 'd' { DICT_BEGIN }
  | 'l' { LIST_BEGIN }
  | 'e' { END }
  | whitespace { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | _ { raise (SyntaxError (
        Printf.sprintf "Unexpected char: %s. Pos: %d"
          (lexeme lexbuf) (lexeme_start lexbuf)
      )) }
  | eof { EOF }

and read_string buf rem =
  parse
  | _ as c {
    Buffer.add_char buf c;
    if rem = 0 then Buffer.contents buf
    else read_string buf (rem - 1) lexbuf
  }
  | eof { raise (SyntaxError ("String not long enough")) }

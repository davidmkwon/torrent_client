%{
open Bencode_types
%}

%token <int64> INT
%token <string> STRING
%token DICT_BEGIN
%token LIST_BEGIN
%token END
%token EOF

%start <Bencode_types.t option> prog

%%

prog:
  | EOF { None }
  | b = bencode { Some b }

bencode:
  | DICT_BEGIN; d = dict; END { Dict d }
  | DICT_BEGIN; END { Dict [] }
  | LIST_BEGIN; b = bencode; END { List b }
  | LIST_BEGIN; END { List [] }
  | s = STRING; {String s}
  | i = INT; { Int i }

dict:
  | s = STRING; b = bencode; { [(s, b)] }
  | s = STRING;

%{
open Types
%}

%token <int64> INT
%token <string> STRING
%token DICT_BEGIN
%token LIST_BEGIN
%token END
%token EOF

%start <Types.t option> prog

%%

prog:
  | EOF { None }
  | b = bencode { Some b }

bencode:
  | DICT_BEGIN; d = dict; END { Dict d }
  | DICT_BEGIN; END { Dict [] }
  | LIST_BEGIN; b = bencodes; END { List b }
  | LIST_BEGIN; END { List [] }
  | s = STRING; {String s}
  | i = INT; { Int i }

bencodes:
  | b = bencode; bs = bencodes { b::bs }
  | b = bencode { [b] }

dict:
  | s = STRING; b = bencode; d = dict { (s, b)::d }
  | s = STRING; b = bencode; { [(s, b)] }

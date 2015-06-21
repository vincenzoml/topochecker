{
  open Parser        (* The type token is defined in parser.mli *)
  exception Eof
}
rule token = parse
(* Special characters *)
| ['\n' ' ' '\t']     { token lexbuf }     (* skip blanks *)          
| ';'                 { EOL }
| ','                 { COMMA }
| "'"                 { QUOTE }
| '$'                 { DOLLAR }
| '@'                 { AT }
| '<'                 { LBANGLE }
| '>'                 { RBANGLE }
| '('                 { LBROUND }
| ')'                 { RBROUND }
| '['                 { LBSQUARE }
| ']'                 { RBSQUARE }

(* Special names *)
| "image"        { IMAGE }
| "status"       { STATUS }
| "store"        { STORE }
| "time"         { TIME }
| "space"        { SPACE }
| "future"       { FUTURE }
| "formula"      { FORMULA }

(* Formulas *)
| "T"            { T }
| "F"            { F }
| "!"            { NOT }
| "&"            { AND }
| "|"            { OR }
| "A"           { A }
| "E"           { E }
| "X"           { X }
| "G"           { G }
| "U"           { U }
| "N"            { N }
| "S"            { S }

(* Colours *)
| "RED"          { RED }
| "GREEN"        { GREEN }
| "BLUE"         { BLUE }
| "BLACK"         { BLACK }
| "WHITE"         { WHITE }
| "YELLOW"         { YELLOW }
| "CYAN"         { CYAN }
| "MAGENTA"         { MAGENTA }
| "="            { EQ }

(* Commands of the interpreter *)
| "set"          { SET }
| "show"         { SHOW }
| "save"         { SAVE }
| "load"         { LOAD }
| "reset"        { RESET }
| "refresh"      { REFRESH }
| "exit"         { EXIT }
| "sem"          { SEM }
| "let"          { LET }
| eof            { raise Eof }

(* Identifiers and integers *)
| ['0'-'9']+ as lxm {INT (int_of_string lxm)}
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm {IDE lxm}

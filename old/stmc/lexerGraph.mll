{
  open ParserGraph       (* The type token is defined in parser.mli *)
}
rule token = parse
(* fine riga e caratteri speciali*)
| ['\n' ' ' '\t']     { token lexbuf }     (* skip blanks *)          
| ';'                 { SEMICOLON }
| ','                 { COMMA }
| "'"                 { QUOTE }
| '('                 { LBROUND }
| ')'                 { RBROUND }
| '['                 { LBSQUARE }
| ']'                 { RBSQUARE }
| '{'                 { LBCURLY }
| '}'                 { RBCURLY }
| "->"                { RARROW }

(* oggetti *)
| "digraph"           { DIGRAPH }



(* comandi *)
| eof            { EOF }

(* identificatori *)
| ['0'-'9']+ as lxm {INT (int_of_string lxm)}
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm {IDE lxm}

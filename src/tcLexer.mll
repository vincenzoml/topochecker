{
  open TcParser
  exception Eof
}
rule token = parse
  '"' { STRING (stringl (Buffer.create 30) lexbuf) }
| [' ' '\t' '\n'] { token lexbuf }    
| "Gr" {GROUP}
| "-<" {SHARE}
| "Let" {LET}
| "," {COMMA}
| ";" {EOL}
| "(" {LPAREN}
| ")" {RPAREN}
| "[" {LBRACKET}
| "]" {RBRACKET}
| "{" {LCURLY}
| "}" {RCURLY}
| ['"'] {QUOTE}
| "T" {TRUE}
| "F" {FALSE}
| ['0'-'9']+ as lxm {NUM (int_of_string lxm)}
| "&" {AND}
| "|" {OR}
| "!" {NOT}
| "N" {NEAR}
| "I" {INTERIOR}
| "S" {SURROUNDED}
| "->" {ARROW}
| "=" {EQ}
| "^" {HAT}
| "Check" {CHECK}
| "Kripke" {KRIPKE}
| "Space" {SPACE}
| "Eval" {EVAL}
| ['>' '<' '+' '-' '*' '/']+ as lxm {BINOP lxm}
| "E" {E}
| "A" {A}
| "U" {U}
| "G" {G}
| "F" {F}
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm {IDE lxm} 
| eof {raise Eof}

and  stringl buffer = parse
 | '"' { Buffer.contents buffer }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }	

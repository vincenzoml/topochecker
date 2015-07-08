{
  open TcParser
  exception Eof
}
  rule token = parse
  [' ' '\t' '\n'] { token lexbuf }    
| "//"[^'\n']* { token lexbuf } 
| '"' { STRING (stringl (Buffer.create 30) lexbuf) }
| "E" {E}
| "A" {A}
| "U" {U}
| "G" {G}
| "F" {F}
| "X" {X}
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
| "TT" {TRUE}
| "FF" {FALSE}
| ['0'-'9']+'.'['0'-'9']* as lxm {FLOAT (float_of_string lxm)}
| ['0'-'9']+ as lxm {INT (int_of_string lxm)}
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
| (">" | "<"| "=="| "<=" |">"| ">="| "!=") as lxm {OP lxm}
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm {IDE lxm} 
| eof {raise Eof}

and  stringl buffer = parse
 | '"' { Buffer.contents buffer }
 | eof { raise End_of_file }
 | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }	

{
  open Parser
  exception Eof
}
rule token = parse
  [' ' '\t' '\n'] { token lexbuf }    
| "Ask" {ASK}
| "G" {GROUP}
| "-<" {SHARE}
| "Paint" {PAINT}
| "Let" {LET}
| "Reload" {RESET}
| "RED" {RED}
| "GREEN" {GREEN}
| "BLUE" {BLUE}
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
| "N" {CLOS}
| "I" {INT}
| "S" {UNTIL}
| "->" {ARR}
| "=" {EQ}
| "#" {HASH}
| "~" {UNOP "~"}
| "^" {HAT}
| ['>' '<' '+' '-' '*' '/']+ as lxm {BINOP lxm}
| ['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9']* as lxm {IDE lxm} 
| eof {raise Eof}




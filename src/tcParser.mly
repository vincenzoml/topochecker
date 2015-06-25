%token EOL
%token <string> STRING 
%token GROUP
%token SHARE
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token TRUE
%token FALSE
%token AND
%token E
%token A
%token U
%token G
%token X
%token F
%token OR
%token NOT
%token NEAR
%token INTERIOR
%token SURROUNDED
%token <string> UNOP
%token <string> BINOP
%token ARROW
%token HAT
%token COMMA
%token QUOTE
%token <int> NUM
%token <string> IDE
%token LET
%token EQ
%token COMMENT_START
%token LCURLY
%token RCURLY
%token KRIPKE
%token SPACE
%token EVAL       
%token CHECK
%token CCHECK       
%start main
%type <Syntax.experiment> main
%%
main:
| modelSpec declSpec comSpec {($1,$2,$3)}
| modelSpec comSpec {($1,[],$2)}
;  
modelSpec:
| KRIPKE STRING SPACE STRING EVAL STRING eol {Syntax.MODEL ($2,$4,$6)}
;
declSpec:
| decl {[$1]}
| decl eol declSpec {$1 :: $3}    
;
decl:
| LET IDE EQ formula eol {Syntax.LET ($2,[],$4)}
| LET IDE formalarglist EQ formula eol {Syntax.LET ($2,$3,$5)}
;
comSpec:
| CHECK formula eol {Syntax.CHECK $2}
/*| CCHECK formula eol {Syntax.CCHECK $2}
| CCHECK formula set eol {Syntax.CCHECKSET ($2,$3)}*/
;
eol:
| EOL {}
;
set:
| LCURLY point_list RCURLY { $2 }
| LCURLY RCURLY {[]}
;
point_list:
| point { [$1] }
| point COMMA point_list { $1 :: $3 }
;
point:
| LPAREN NUM COMMA NUM RPAREN { ($2,$4) }
;
;       
formula:
| LPAREN formula RPAREN {$2}
| TRUE {Syntax.TRUE}
| FALSE {Syntax.FALSE}
| IDE {Syntax.CALL ($1,[])}
| IDE actualarglist {Syntax.CALL ($1,$2)}
| LBRACKET IDE RBRACKET {Syntax.PROP $2}
| NOT formula {Syntax.NOT $2}
| formula AND formula {Syntax.AND ($1,$3)}
| formula OR formula {Syntax.OR ($1,$3)}
| NEAR formula {Syntax.NEAR $2}
| NEAR HAT NUM formula {Syntax.NEARN ($3,$4)}
| INTERIOR formula {Syntax.INT $2}
| formula SURROUNDED formula {Syntax.SURROUNDED ($1,$3)}
/* | GROUP formula { Syntax.GROUP $2 }
| formula SHARE formula { Syntax.SHARE ($1,$3) } */
| E X formula {Syntax.EX $3}
| A X formula {Syntax.AX $3}
| E G formula {Syntax.EG $3}
| A G formula {Syntax.AG $3}
| E F formula {Syntax.EF $3}
| A F formula {Syntax.AF $3}
| E formula U formula {Syntax.EU ($2,$4)}
| A formula U formula {Syntax.AU ($2,$4)}
;
formalarglist:
| LPAREN innerformalarglist RPAREN {$2}
;
innerformalarglist:
| IDE {[$1]}
| IDE COMMA innerformalarglist {$1::$3}
;
actualarglist:
| LPAREN inneractualarglist RPAREN {$2}
;
inneractualarglist:
| formula {[$1]}
| formula COMMA inneractualarglist {$1::$3}
;


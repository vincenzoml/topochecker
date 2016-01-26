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
%token <string> OP
%token ARROW
%token HAT
%token COMMA
%token QUOTE
%token <float> FLOAT
%token <int> INT
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
%token ASK
%token OUTPUT
%token EOF 
%start main
%type <Syntax.experiment> main
%%
main:
| modelSpec declSpec comSpec EOF {($1,$2,$3)}
| modelSpec comSpec EOF {($1,[],$2)}
; 
modelSpec:
| KRIPKE STRING SPACE STRING EVAL STRING eol {Syntax.MODEL ($2,$4,$6)}
;
declSpec:
| decl {[$1]}
| decl declSpec {$1 :: $2}    
;
decl:
| LET IDE EQ formula eol {Syntax.LET ($2,[],$4)}
| LET IDE formalarglist EQ formula eol {Syntax.LET ($2,$3,$5)}
;
comSpec:
| com {[$1]}
| com comSpec {$1 :: $2}
;
com:
| ASK qformula eol {Syntax.ASK $2}
| CHECK STRING formula eol {Syntax.CHECK ($2,$3)}
| OUTPUT STRING eol {Syntax.OUTPUT ($2,None)};
| OUTPUT STRING states eol {Syntax.OUTPUT ($2,Some $3)} 
;
states:
| STRING { [$1] }
| STRING COMMA states {$1::$3}
eol:
| EOL {}
;
qformula:
  TRUE {Syntax.QFALSE}
| FALSE {Syntax.QTRUE}
| LPAREN qformula RPAREN { $2 }
| NOT qformula { Syntax.QNOT $2 }
| qformula AND qformula { Syntax.QAND ($1,$3) }
| qformula OR qformula { Syntax.QOR ($1,$3) }
| qaformula OP qaformula { Syntax.QOP ($2,$1,$3) }
;
qaformula:
  formula { Syntax.QFSYN $1 }
| INT { Syntax.QINT $1 }
;
formula:
| LPAREN formula RPAREN {$2}
| TRUE {Syntax.TRUE}
| FALSE {Syntax.FALSE}
| IDE {Syntax.CALL ($1,[])}
| IDE actualarglist {Syntax.CALL ($1,$2)}
| LBRACKET IDE RBRACKET {Syntax.PROP $2}
| LBRACKET IDE OP FLOAT RBRACKET {Syntax.VPROP ($2,$3,$4)}
| LBRACKET IDE OP INT RBRACKET {Syntax.VPROP ($2,$3,(float_of_int $4))}
| NOT formula {Syntax.NOT $2}
| formula AND formula {Syntax.AND ($1,$3)}
| formula OR formula {Syntax.OR ($1,$3)}
| NEAR formula {Syntax.NEAR $2}
| NEAR HAT INT formula {Syntax.NEARN ($3,$4)}
| INTERIOR formula {Syntax.INT $2}
| formula SURROUNDED formula {Syntax.SURROUNDED ($1,$3)}
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

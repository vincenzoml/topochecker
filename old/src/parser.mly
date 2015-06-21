%token EOL
%token ASK
%token GROUP
%token SHARE
%token HASH
%token RESET
%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token TRUE
%token FALSE
%token AND
%token OR
%token NOT
%token CLOS
%token INT
%token UNTIL
%token <string> UNOP
%token <string> BINOP
%token RED
%token GREEN
%token BLUE
%token ARR
%token HAT
%token COMMA
%token QUOTE
%token <int> NUM
%token PAINT
%token <string> IDE
%token LET
%token EQ
%token COMMENT_START
%token COMMENT_END
%token LCURLY
%token RCURLY
%start main
%type <Ccsmc.PictureLogic.syntax> main
%%
  main:
 | PAINT color formula eol {Ccsmc.PictureLogic.PAINT (Ccsmc.Picture.COL $2,$3)}
 | ASK cformula eol {Ccsmc.PictureLogic.ASK $2}
 | ASK cformula set eol {Ccsmc.PictureLogic.ASKSET ($2,$3)}
 | LET IDE EQ formula eol {Ccsmc.PictureLogic.LET ($2,[],$4)}
 | LET IDE formalarglist EQ formula eol {Ccsmc.PictureLogic.LET ($2,$3,$5)} 
 | RESET eol {Ccsmc.PictureLogic.RESET}
 ;
   eol:
 | EOL {}
 ;
   set:
     LCURLY point_list RCURLY { $2 }
 | LCURLY RCURLY {[]}
 ;
   point_list:
     point { [$1] }
 | point COMMA point_list { $1 :: $3 }
 ;
   point:
     LPAREN NUM COMMA NUM RPAREN { ($2,$4) }
 ;
   cformula:
 | LPAREN cformula RPAREN {$2}
 | TRUE { Ccsmc.PictureLogic.CTRUE }
 | FALSE { Ccsmc.PictureLogic.CFALSE }
 | NOT cformula { Ccsmc.PictureLogic.CNOT $2 }
 | cformula AND cformula { Ccsmc.PictureLogic.CAND ($1,$3) }
 | cformula OR cformula { Ccsmc.PictureLogic.COR ($1,$3) }
 | GROUP formula { Ccsmc.PictureLogic.GROUP $2 }
 | formula SHARE cformula { Ccsmc.PictureLogic.SHARE ($1,$3) }
  ;       
  formula:
 | LPAREN formula RPAREN {$2}
 | TRUE {Ccsmc.PictureLogic.TRUE}
 | FALSE {Ccsmc.PictureLogic.FALSE}
 | IDE {Ccsmc.PictureLogic.CALL ($1,[])}
 | IDE actualarglist {Ccsmc.PictureLogic.CALL ($1,$2)}
 | LBRACKET prop RBRACKET {Ccsmc.PictureLogic.PROP $2}
 | NOT formula {Ccsmc.PictureLogic.NOT $2}
 | formula AND formula {Ccsmc.PictureLogic.AND ($1,$3)}
 | formula OR formula {Ccsmc.PictureLogic.OR ($1,$3)}
 | CLOS formula {Ccsmc.PictureLogic.CLOS $2}
 | CLOS HAT NUM formula {Ccsmc.PictureLogic.CLOSN ($3,$4)}
 | INT formula {Ccsmc.PictureLogic.INT $2}
 | formula UNTIL formula {Ccsmc.PictureLogic.UNTIL ($1,$3)}
  ;
  formalarglist:
    LPAREN innerformalarglist RPAREN {$2}
  ;
  innerformalarglist:
 | IDE {[$1]}
 | IDE COMMA innerformalarglist {$1::$3}
  ;
  actualarglist:
    LPAREN inneractualarglist RPAREN {$2}
  ;
  inneractualarglist:
 | formula {[$1]}
 | formula COMMA inneractualarglist {$1::$3}
  ;
  prop:
 | color {Ccsmc.Picture.COL $1}
 | UNOP color {Ccsmc.Picture.UNOP ($1,$2)}
 | value BINOP value {Ccsmc.Picture.BINOP ($1,$2,$3)}
 | value EQ value {Ccsmc.Picture.BINOP ($1,"=",$3)} 
  ;
  color:
 | QUOTE IDE QUOTE {Ccsmc.Picture.COLOR $2}
 | HASH NUM NUM NUM {Ccsmc.Picture.RGB ($2,$3,$4)}
  ;
  value:
 | RED {Ccsmc.Picture.RED}
 | GREEN {Ccsmc.Picture.GREEN}
 | BLUE {Ccsmc.Picture.BLUE}
 | NUM {Ccsmc.Picture.NUM $1}


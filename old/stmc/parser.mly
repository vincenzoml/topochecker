%{
  open Interface
%}
%token EOL
%token QUOTE
%token COMMA
%token DOLLAR
%token AT
%token IMAGE
%token STATUS
%token STORE
%token FUTURE
%token TIME
%token SPACE
%token FORMULA
%token SET
%token SHOW
%token SEM
%token LET
%token RED
%token GREEN
%token BLUE
%token BLACK
%token WHITE
%token YELLOW
%token CYAN
%token MAGENTA
%token EQ
%token T
%token F
%token NOT
%token AND
%token OR
%token X
%token G
%token U
%token A
%token E
%token N
%token S
%token LBROUND
%token RBROUND
%token LBANGLE
%token RBANGLE
%token LBSQUARE
%token RBSQUARE
%token SAVE
%token LOAD
%token RESET
%token REFRESH
%token EXIT
%token <string> IDE
%token <int> INT
%start main
%type <Interface.MyModel.st_pointset Interface.MyLogic.fsyntax Interface.command> main
%%
main:
command EOL                { $1 }
  ;
  command:
 | SHOW showarg                 { $2 }
 | SET setarg                   { $2 }
 | SEM semarg                   { $2 }
 | LET IDE EQ fsyntax           { LET ($2,[],$4) }
 | LET IDE LBROUND arglist RBROUND EQ fsyntax { LET ($2,$4,$7) }  
 | SAVE savearg                 { $2 }
 | LOAD loadarg                 { $2 }
 | RESET                        { RESET }
 | REFRESH                      { REFRESH }
 | EXIT                         { STOP_TEST }
  ;
  savearg:
 | STORE              { SAVE_STORE }
 | IMAGE IDE          { SAVE_IMAGE $2 }
 | IMAGE INT IDE      { SAVE_SINGLE_IMAGE ($2,$3) }
  ;
  loadarg:
 | STORE              { LOAD_STORE }
  ;
  semarg:
 | color fsyntax            { SEM($1,$2) }
  ;
  showarg:
 | STATUS                       { SHOW_STATUS }
 | FORMULA                      { SHOW_FORMULA }
 | STORE                        { SHOW_STORE }
 | SPACE                        { SHOW_SPACE Graphics.red }
 | SPACE color                  { SHOW_SPACE $2 }
 | TIME                         { SHOW_TIME }
 | FUTURE                       { SHOW_FUTURE }
  ;
  setarg:
 | TIME INT                 { SET_TIME $2 }
 | SPACE INT INT            { SET_SPACE ($2,$3) }
  ;
  fsyntax:
 | LBROUND fsyntax RBROUND                           { $2 }
 | T                                                 { MyLogic.TRUE }
 | F                                                 { MyLogic.FALSE }
 | LBANGLE propcolor RBANGLE                         { MyLogic.PROP $2 }
 | NOT fsyntax                                       { MyLogic.NOT ($2) }
 | fsyntax AND fsyntax                               { MyLogic.AND ($1,$3) }
 | fsyntax OR fsyntax                                { MyLogic.OR ($1,$3) }
 | N fsyntax                                         { MyLogic.NEAR $2 }
 | fsyntax S fsyntax                                 { MyLogic.SURR ($1,$3) }
 | A X fsyntax                                        { MyLogic.AX $3 }
 | E X fsyntax                                        { MyLogic.EX $3 }
 | A F fsyntax                                        { MyLogic.AF $3 }
 | E F fsyntax                                        { MyLogic.EF $3 }
 | A G fsyntax                                        { MyLogic.AG $3 }
 | E G fsyntax                                        { MyLogic.EG $3 }
 | A fsyntax U fsyntax                               { MyLogic.AU ($2,$4) }
 | E fsyntax U fsyntax                               { MyLogic.EU ($2,$4) }
 | IDE                                               { MyLogic.CALL ($1,[]) }
 | IDE LBROUND formulalist RBROUND                   { MyLogic.CALL ($1,$3) }
  ;

  arglist:
 | IDE                { $1::[] }
 | IDE arglist        { $1::$2 }
  ;
  formulalist:
 | fsyntax                      { $1::[] }
 | fsyntax COMMA formulalist    { $1::$3 }
  ;
  color:
 | RED                                         { Graphics.red }
 | GREEN                                       { Graphics.green }
 | BLUE                                        { Graphics.blue }
 | BLACK                                       { Graphics.black }
 | WHITE                                       { Graphics.white }
 | YELLOW                                      { Graphics.yellow }
 | CYAN                                        { Graphics.cyan }
 | MAGENTA                                     { Graphics.magenta }
 | LBROUND INT COMMA INT COMMA INT RBROUND     { Graphics.rgb $2 $4 $6 }
  ;
  propcolor:
 | IDE                                         { MyProp.Id $1 }
 | RED LBSQUARE INT COMMA INT RBSQUARE         { MyProp.RedRange ($3,$5) }
 | GREEN LBSQUARE INT COMMA INT RBSQUARE       { MyProp.GreenRange ($3,$5) }
 | BLUE LBSQUARE INT COMMA INT RBSQUARE        { MyProp.BlueRange ($3,$5) }

%{
  open Graph
  open Interface

  let rec add_node_list = fun nl time ->
    match nl with
    | [] -> time
    | x::xs -> add_node_list xs (MyTimeGraph.add_node x time)

  let rec add_arc_list = fun al time ->
    match al with
    | [] -> time
    | (a,b)::abs -> add_arc_list abs (MyTimeGraph.add_arc a b time)

%}
%token EOF
%token QUOTE
%token COMMA
%token SEMICOLON
%token LBROUND
%token RBROUND
%token LBCURLY
%token RBCURLY
%token LBSQUARE
%token RBSQUARE
%token RARROW
%token DIGRAPH
%token EXIT
%token <string> IDE
%token <int> INT
%start main
%type <Interface.MyTimeGraph.t> main
%%
  main:
    DIGRAPH LBCURLY grapharg RBCURLY EOF  { $3 }
    ;

      grapharg:
    | grapharg arcdescription SEMICOLON  { let (nl,al) = $2 in
					  add_arc_list al (add_node_list nl $1)
					}
    | arcdescription SEMICOLON      { let (nl,al) = $1 in
				      add_arc_list al (add_node_list nl MyTimeGraph.empty)
				    }
    ;
      arcdescription:
    | INT RARROW arcdescription  { let (nl,al) = $3 in
				   ($1::nl,($1,List.hd nl)::al)
				 }
    | INT                        { ($1::[],[]) }
				 

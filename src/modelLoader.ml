open Graph.Dot_ast
open Bigarray

module StringSet = Set.Make(String)
       
module ParserSig =
  struct
    let node (id,_) _ =
      match id with
	Number n -> int_of_string n
      | _ -> Util.fail "Node ids must be integer"
    let edge _ = ()
  end
    
module Parser = Graph.Dot.Parse(Graph.Builder.I(Model.Graph))(ParserSig)

let parse_eval filename states points =
  let prop_tbl = (Hashtbl.create 100 : (Logic.formula,Model.slice) Hashtbl.t) in
  let chan = open_in filename in
  let csv_chan = Csv.of_channel ~separator:' ' chan in
  (try
      while true do
	match Csv.next csv_chan with
	  stateS::pointS::props ->
	  let (state,point) = (int_of_string stateS,int_of_string pointS) in
	  List.iter
	    (fun prop ->
	     let a = try Hashtbl.find prop_tbl (Logic.Prop prop)
		     with Not_found -> let a = Array2.create int c_layout states points in
				       Array2.fill a Util.valFalse;
				       Hashtbl.add prop_tbl (Logic.Prop prop) a;
				       a
	     in
	     Array2.set a state point Util.valTrue) props;
	| _ -> Util.fail "each line in the csv file of the evaluation function must have at least two columns"
      done
    with
      End_of_file -> ()
    | Csv.Failure (_,_,_) (* TODO: use error info *) -> Util.fail "wrong csv format of the evaluation function"
    | Failure "int_of_string" -> Util.fail "states and points must be integers in the evaluation function"
  );
  Csv.close_in csv_chan;
  prop_tbl
      
let load_model : string -> string -> string -> string Model.model =
  fun kripkef spacef evalf ->
  let kripke = Parser.parse kripkef in
  let space = Parser.parse spacef in
  let propTbl = parse_eval evalf (Model.Graph.nb_vertex kripke) (Model.Graph.nb_vertex space) in
  Model.Graph.iter_vertex (fun v -> if 0 = Model.Graph.out_degree kripke v then Model.Graph.add_edge kripke v v) kripke;
  { Model.kripke = kripke;
    Model.space = space;
    Model.eval = propTbl; }
    
let mkfname dir file =
  if Filename.is_relative file then dir ^ Filename.dir_sep ^ file else file							      

let load_experiment path =  
  let (dir,file) = (Filename.dirname path,Filename.basename path) in  
  let lexbuf = Lexing.from_channel (open_in (mkfname dir file)) in
  try
    let (syntax,dseq,command) = TcParser.main TcLexer.token lexbuf in
    match (syntax,dseq,command) with
      (Syntax.MODEL (kripkef,spacef,evalf),dseq,Syntax.CHECK fsyn) ->
      let model = load_model (mkfname dir kripkef) (mkfname dir spacef) (mkfname dir evalf) in
      let env = Syntax.env_of_dseq dseq in
      let formula = Syntax.formula_of_fsyn env fsyn in
      (model,formula)
  with exn -> 
    let msg = Printexc.to_string exn in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Util.fail (Printf.sprintf "filename: %s, line %d, character %d, token %s: %s\n%!" file line cnum tok msg)
	      

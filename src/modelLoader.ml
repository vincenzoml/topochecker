open Graph.Dot_ast
open Bigarray

module StringSet = Set.Make(String)

let list = ref []

let debug_edge x =
  match x with
    (Number s|String s|Html s|Ident s) -> Util.debug s

let string_of_id i =
  match i with
    (String s | Html s | Ident s | Number s) -> s

module IntMap = Map.Make(struct type t = int let compare = compare end)
						  
module ParserSig =
  struct
    let (mkId,reset,read) =
      let h = ref (Hashtbl.create 1000) in
      let idTbl = ref IntMap.empty in
      let curId = ref 0 in
      ((fun id ->
	let x = !curId in
	curId := x+1;
	idTbl := IntMap.add x id !idTbl;
	Hashtbl.add (!h) id x;
	x),
       (fun () ->
	idTbl := IntMap.empty;
	h := Hashtbl.create 1000;
	curId := 0),
       (fun () -> let (t,h) = (!idTbl,!h) in
		  ((fun x -> IntMap.find x t),(fun x -> Hashtbl.find h x))))
	
    let node (id,_) _ =
      match id with
	(Ident s|Number s|String s|Html s) ->
	mkId s

    let edge l =
      list := l::!list;
      match l with	
	[(i1,Some i2)]::_ ->
	if string_of_id i1 = "weight"
	then float_of_string (string_of_id i2)
	else Model.Edge.default
      | _ -> Model.Edge.default
  end
    
module Parser = Graph.Dot.Parse(Graph.Builder.I(Model.Graph))(ParserSig)

let parse_eval filename states points stateid pointid =
  let prop_tbl = Model.H.create 100 in
  let chan = open_in filename in
  let csv_chan = Csv.of_channel ~separator:',' chan in
  (try
      while true do
	let n = Csv.next csv_chan in
	match n with
	  stateS::pointS::props ->
	  let (state,point) = (stateid stateS,pointid pointS) in
	  List.iter
	    (fun datum ->
	     let (prop,value) =
	       match Str.split (Str.regexp "=") datum with
		 [prop] -> (prop,Util.valTrue)
	       | [prop;v] -> (prop,float_of_string v)
	       | _ -> Util.fail "Atomic propositions in csv file must be of the form pro' or prop=value where prop is a string and value is an integer"
	     in
	     let a = try Model.H.find prop_tbl (Logic.Prop prop)
		     with Not_found -> let a = Array2.create float64 c_layout states points in
				       Array2.fill a Util.valFalse;
				       Model.H.add prop_tbl (Logic.Prop prop) a;
				       a
	     in
	     Array2.set a state point value
	    ) props;
	| _ -> Util.fail "each line in the csv file of the evaluation function must have at least two columns"
      done
    with
      End_of_file -> ()
    | Csv.Failure (_,_,_) (* TODO: use error info *) -> Util.fail "wrong csv format of the evaluation function"
    | Failure "int_of_string" -> Util.fail "states and points must be integers in the evaluation function"
  );
  Csv.close_in csv_chan;
  prop_tbl

let completeDeadlocks kripke =
  let vect = Array1.create float64 c_layout (Model.Graph.nb_vertex kripke) in
  Array1.fill vect Util.valFalse;
  Model.Graph.iter_vertex (fun v -> if 0 = Model.Graph.out_degree kripke v
				    then begin
					Model.Graph.add_edge kripke v v;
					Array1.set vect v Util.valTrue
				      end)
			  kripke;
  fun state -> Util.isTrue (Array1.get vect state)
  
let load_model : string -> string -> string -> Model.model =
  fun kripkef spacef evalf ->
  let kripke = Parser.parse kripkef in
  let (k_id_of_int,k_int_of_id) = ParserSig.read () in      
  ParserSig.reset ();
  let space = Parser.parse spacef in
  let (s_id_of_int,s_int_of_id)  = ParserSig.read () in (* TODO remove k_h *)
  ParserSig.reset ();
  let propTbl = parse_eval evalf (Model.Graph.nb_vertex kripke) (Model.Graph.nb_vertex space) k_int_of_id s_int_of_id in  
  let deadlocks = completeDeadlocks kripke in
  { Model.kripke = kripke;
    Model.space = space;
    Model.deadlocks = deadlocks;
    Model.kripkefname = kripkef;
    Model.spacefname = spacef;
    Model.kripkeid = k_id_of_int;
    Model.idkripke = k_int_of_id;
    Model.spaceid = s_id_of_int;
    Model.idspace = s_int_of_id;
    Model.eval = propTbl; }
    
let mkfname dir file =
  if Filename.is_relative file then dir ^ Filename.dir_sep ^ file else file	
						      
type command =
    Check of int * Logic.formula
  | Output of string * (int list option)
									 
let load_experiment =
  fun path ->
  let (dir,file) = (Filename.dirname path,Filename.basename path) in
  let desc = open_in (mkfname dir file) in
  let lexbuf = Lexing.from_channel desc in
  try
    let (Syntax.MODEL (kripkef,spacef,evalf),dseq,commands) = TcParser.main TcLexer.token lexbuf in
    let model = load_model (mkfname dir kripkef) (mkfname dir spacef) (mkfname dir evalf) in
    let env = Syntax.env_of_dseq dseq in
    let commands = List.map (function
				Syntax.CHECK (color,fsyn) -> Check (int_of_string color,Syntax.formula_of_fsyn env fsyn)
			      | Syntax.OUTPUT (s,None) -> Output (s,None)
    			      | Syntax.OUTPUT (s,Some states) -> Output (s,Some (List.map model.Model.idkripke states))) commands in
    close_in desc; (* TODO use a safe wrapper for the open/close pairs here and everywhere else *)
    (model,commands)
  with exn ->
    close_in desc;
    let msg = Printexc.to_string exn in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Util.fail (Printf.sprintf "filename: %s, line %d, character %d, token %s: %s\n%!" file line cnum tok msg)
	      

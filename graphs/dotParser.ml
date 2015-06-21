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
    
module Parser = Graph.Dot.Parse(Graph.Builder.I(Model.Space))(ParserSig)

let parse_eval filename states points =
  let prop_tbl = (Hashtbl.create 100 : (Logic.formula,Slice.t) Hashtbl.t) in
  let chan = open_in filename in
  let csv_chan = Csv.of_channel chan in
  (try
      while true do
	match Csv.next csv_chan with
	  stateS::pointS::props ->
	  let (state,point) = (int_of_string stateS,int_of_string pointS) in
	  List.iter
	    (fun prop ->
	     let a = try Hashtbl.find prop_tbl (Logic.Prop prop)
		     with Not_found -> Array2.create int c_layout states points in
	     Array2.fill a IntBool.valFalse;
	     Array2.set a state point IntBool.valTrue) props;
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
  let propTbl = parse_eval evalf (Model.Space.nb_vertex kripke) (Model.Space.nb_vertex space) in
  { Model.kripke = kripke;
    Model.space = space;
    Model.eval = propTbl; }
    




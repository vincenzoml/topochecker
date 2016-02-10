open Graph.Dot_ast
open Bigarray

type dot_data =
  { spacefname : string }

let string_of_id i =
  match i with
    (String s | Html s | Ident s | Number s) -> s
						  
module ParserSig =
  struct
    let (mkId,reset,read) =
      let h = ref (Hashtbl.create 10000) in
      let idTbl = ref (Hashtbl.create 10000) in
      let curId = ref 0 in
      ((fun id ->
	let x = !curId in
	curId := x+1;
	Hashtbl.add (!idTbl) x id;
	Hashtbl.add (!h) id x;
	x),	
       (fun () ->
	 idTbl := Hashtbl.create 10000;
	 h := Hashtbl.create 10000;
	 curId := 0),
       (fun () -> let (t,h) = (!idTbl,!h) in
		  ((fun x -> Hashtbl.find t x),
		   (fun x -> Hashtbl.find h x))))
	
    let node (id,_) _ =
      match id with
	(Ident s|Number s|String s|Html s) ->
	mkId s

    let edge l =
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
	       | _ -> Util.fail
		  "Atomic propositions in csv file must be of the form 'prop' or 'prop=value' where prop is a string and value is a floating point number"
	     in
	     let a = try Model.H.find prop_tbl (Logic.Prop prop)
	       with Not_found ->
		 let a = Array2.create float64 c_layout states points in
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
    | Failure "int_of_string" ->
       Util.fail "states and points must be integers in the evaluation function"
  );
  Csv.close_in csv_chan;
  prop_tbl
        
let load_dot_model dir k s e =
  let (spacef,evalf) =  (Util.mkfname dir s,Util.mkfname dir e) in
  let (kripke,(k_id_of_int,k_int_of_id)) =
    if k = ""
    then (Model.default_kripke (),(string_of_int,int_of_string))
    else (Parser.parse (Util.mkfname dir k),ParserSig.read ())
  in
  ParserSig.reset ();
  let space = Parser.parse spacef in
  let (s_id_of_int,s_int_of_id)  = ParserSig.read () in
  ParserSig.reset ();
  let propTbl = parse_eval evalf (Model.Graph.nb_vertex kripke) (Model.Graph.nb_vertex space) k_int_of_id s_int_of_id in  
  { Model.kripke = kripke;
    Model.space = space;
    Model.deadlocks = None;
    kripkeid = k_id_of_int;
    idkripke = k_int_of_id;
    spaceid = s_id_of_int;
    idspace = s_int_of_id;
    Model.local_state =
      {	spacefname = spacef };
    Model.eval = propTbl }

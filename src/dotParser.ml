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
  let prop_tbl = Model.H.create 10 in
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
  let res = Model.H.create (Model.H.length prop_tbl) in
  Model.H.iter (fun k v -> Model.H.add res k (Array2.get v)) prop_tbl;
  res

module PrinterGraph =
  struct
    include Model.Graph

    let vertex_attributes_fn = ref (fun v -> Util.fail "Printer not initialized")
    let vertex_name_fn = ref (fun v -> Util.fail "Printer not initialized")

    let edge_attributes e = []
    let default_edge_attributes g = [`Dir `Forward]
    let get_subgraph g = None
    let vertex_attributes v = !vertex_attributes_fn v
    let graph_attributes g = []
    let vertex_name v = !vertex_name_fn v
    let default_vertex_attributes g = []			      
  end

module Printer = Graph.Graphviz.Neato(PrinterGraph)

let vertex_color colored_truth_vals state point =
    List.fold_left
      (fun accum (color,truth_val) -> 
       if truth_val state point
       then (int_of_string color) (* accum + color *) 
       else accum)
      0x000000 colored_truth_vals
      
let write_state spacegraph spaceid state colored_truth_vals (output : out_channel) =
    PrinterGraph.vertex_attributes_fn :=
      (fun point ->
       let col = vertex_color colored_truth_vals state point in
       if col == 0 then []
       else [`Color col; `Style `Filled]);
    PrinterGraph.vertex_name_fn := spaceid;
    Printer.output_graph output spacegraph

let alternate_write_state spacefname spacegraph spaceid state colored_truth_vals (output : out_channel) space_fname =
  (* TODO this is a quick hack for quick saving... *)
  let input = open_in space_fname in
  let l1 = input_line input in
  if not (Str.string_match (Str.regexp_case_fold "[' ' '\t']*digraph[^{]*") l1 0)
  then (close_in input; write_state spacegraph spaceid state colored_truth_vals output)
  else
    begin
      Printf.fprintf output "%s\n" l1;
      for point = 0 to Model.Graph.nb_vertex spacegraph - 1 do
	let col = vertex_color colored_truth_vals state point in
	Printf.fprintf output "%d [fillcolor=\"#%06X\",style=\"filled\"];\n" point (if col == 0 then 0xFFFFFF else col);
      done;
      try
	while true do
	  Printf.fprintf output "%s\n" (input_line input)
	done
      with End_of_file -> close_in input
    end

let write_dot_model spacefname kripkegraph spacegraph kripkeid spaceid fname states colored_truth_vals =
  match colored_truth_vals with
    [] -> ()
  | _ ->
     let aux fn =
       match states with
	 None ->
	   for state = 0
	     to Model.Graph.nb_vertex kripkegraph - 1
	   do
	     fn state;
	   done;
       | Some lst -> List.iter fn lst
     in
     let fn state =
       let out_name =  (Printf.sprintf "%s-%s.dot" fname (kripkeid state)) in
       let output = open_out out_name in
       alternate_write_state spacefname spacegraph spaceid state (List.rev colored_truth_vals) output spacefname;
       close_out output
     in
     aux fn
        
let load_dot_model dir k s e =
  let (spacef,evalf) =  (Util.mkfname dir s,Util.mkfname dir e) in
  let (kripke,(k_id_of_int,k_int_of_id)) =
    if k = ""
    then (Model.default_kripke (),(string_of_int,int_of_string))
    else (Parser.parse (Util.mkfname dir k),ParserSig.read ())
  in
  ParserSig.reset ();
  let spaceg = Parser.parse spacef in
  let space = { Model.num_nodes = Model.Graph.nb_vertex spaceg;
		Model.iter_pre = (fun v fn -> Model.Graph.iter_pred (fun x -> fn x 1.0) spaceg v);
		Model.iter_post = (fun v fn -> Model.Graph.iter_succ (fun x -> fn x 1.0) spaceg v) }
  in
  let (s_id_of_int,s_int_of_id)  = ParserSig.read () in
  ParserSig.reset ();
  let ch = Model.CH.create 10 in
  let propTbl = parse_eval evalf (Model.Graph.nb_vertex kripke) (Model.Graph.nb_vertex spaceg) k_int_of_id s_int_of_id in
  { Model.kripke = kripke;
    Model.space = space;
    Model.collective_eval = ch;
    Model.deadlocks = None;
    Model.iter_ball = None;
    Model.write_output = write_dot_model spacef kripke spaceg k_id_of_int s_id_of_int;
    kripkeid = k_id_of_int;
    idkripke = k_int_of_id;
    spaceid = s_id_of_int;
    idspace = s_int_of_id;
    Model.eval = propTbl }
    

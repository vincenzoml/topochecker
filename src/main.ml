module PrinterGraph =
  struct
    include Model.Graph

    let fn = ref (fun v -> Util.fail "Printer not initialized")

    let edge_attributes e = []
    let default_edge_attributes g = [`Dir `Forward]
    let get_subgraph g = None
    let vertex_attributes v = (!fn) v
    let graph_attributes g = []
    let vertex_name v = Printf.sprintf "%d%!" v
    let default_vertex_attributes g = []			      
  end

module Printer = Graph.Graphviz.Neato(PrinterGraph)
				     
let f args =
  let (expfname,outbasefname) =
    try
      (args.(1),args.(2)) 
    with      
      Invalid_argument s ->
      Util.fail (Printf.sprintf "Usage: %s FILENAME OUTPUT_PREFIX\n" Sys.argv.(0))
  in
  let (model,formula) = ExpLoader.load_experiment expfname in  
  let checker = Checker.precompute model in
  let truth_val = checker formula in
  for state = 0 to Model.Graph.nb_vertex model.Model.kripke - 1 do
    let out_name =  (Printf.sprintf "%s-%02d.dot" outbasefname state) in
    let output = open_out out_name in
    PrinterGraph.fn := (fun point ->
			if truth_val state point then [`Color 0xFF0000; `Style `Filled] else []);
    Printer.output_graph output model.Model.space;
    close_out output
  done

let _ = f Sys.argv

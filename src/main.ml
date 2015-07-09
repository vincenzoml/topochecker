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
				     
let main args =
  let (expfname,outbasefname) =
    try
      (args.(1),args.(2)) 
    with      
      Invalid_argument s ->
      Util.fail (Printf.sprintf "Usage: %s FILENAME OUTPUT_PREFIX\n" Sys.argv.(0))
  in
  Util.debug "Step 1/3: Loading experiment...";
  let (model,colored_formulas) = ModelLoader.load_experiment expfname in
  Util.debug "Step 2/3: Precomputing model checking table...";
  let checker = Checker.precompute model in
  let colored_truth_vals = List.map (fun (color,formula) -> (color,checker formula)) colored_formulas in
  Util.debug "Step 3/3: Writing output files...";
  for state = 0 to Model.Graph.nb_vertex model.Model.kripke - 1 do
    let out_name =  (Printf.sprintf "%s-%s.dot" outbasefname (model.Model.kripkeid state)) in
    let output = open_out out_name in 
    PrinterGraph.vertex_attributes_fn := (fun point ->
					  let col =
					    List.fold_left
					      (fun accum (color,truth_val) -> 
					       if truth_val state point
					       then accum
					       else accum + color)
					      0x000000 colored_truth_vals
					  in
					  if col == 0 then []
					  else [`Color col; `Style `Filled]);
    PrinterGraph.vertex_name_fn := model.Model.spaceid;
    Printer.output_graph output model.Model.space;
    close_out output
  done;
  Util.debug "All done."

let _ = main Sys.argv

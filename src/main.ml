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
       then color (* accum + color *) 
       else accum)
      0x000000 colored_truth_vals
      
let write_state model state colored_truth_vals (output : out_channel) =
    PrinterGraph.vertex_attributes_fn :=
      (fun point ->
       let col = vertex_color colored_truth_vals state point in
       if col == 0 then []
       else [`Color col; `Style `Filled]);
    PrinterGraph.vertex_name_fn := model.Model.spaceid;
    Printer.output_graph output model.Model.space

let alternate_write_state model state colored_truth_vals (output : out_channel) space_fname =
  (* TODO this is a quick hack *)
  let input = open_in space_fname in
  let l1 = input_line input in
  if not (Str.string_match (Str.regexp_case_fold "[' ' '\t']*digraph[^{]*") l1 0)
  then (Util.debug "didn't match"; write_state model state colored_truth_vals output)
  else
    begin
      Printf.fprintf output "%s\n" l1;
      for point = 0 to Model.Graph.nb_vertex model.Model.space - 1 do
	let col = vertex_color colored_truth_vals state point in
	Printf.fprintf output "%d [fillcolor=\"#%06X\",style=\"filled\"];\n" point (if col == 0 then 0xFFFFFF else col)
      done;
      try
	while true do
	  Printf.fprintf output "%s\n" (input_line input)
	done
      with End_of_file -> ()
    end
      
let main args =
  let (expfname,outbasefname) =
    try
      (args.(1),(try Some args.(2) with _ -> None)) 
    with      
      Invalid_argument s ->
      Util.fail (Printf.sprintf "Usage: %s FILENAME OUTPUT_PREFIX\n" Sys.argv.(0))
  in
  Util.debug "Step 1/3: Loading experiment...";
  let (model,commands) = ModelLoader.load_experiment expfname in
  (* TODO: check for output commands here! *)
  Util.debug "Step 2/3: Precomputing model checking table...";
  let checker = Checker.precompute model in
  let products =
    List.fold_left
      (fun accum command ->
       match command with
	 ModelLoader.Check (color,formula) ->
	 (match accum with
	    [] ->
	    (match outbasefname with
	       None -> Util.fail "no filename specified either on command line or in experiment file"
	     | Some fname -> [(fname,[(color,(checker formula))])])
	  | (fname,fmlas)::rem -> (fname,(color,(checker formula))::fmlas)::rem)
       | ModelLoader.Output fname ->
	  (fname,[])::accum)
      [] commands 
  in		   
  Util.debug "Step 3/3: Writing output files...";
  List.iter
    (fun (fname,colored_truth_vals) ->
     match colored_truth_vals with
       [] -> ()
     | _ ->
	for state = 0 to Model.Graph.nb_vertex model.Model.kripke - 1 do
	  let out_name =  (Printf.sprintf "%s-%s.dot" fname (model.Model.kripkeid state)) in
	  let output = open_out out_name in
	  alternate_write_state model state (List.rev colored_truth_vals) output model.Model.spacefname;
	  close_out output
	done)
    products;
  Util.debug "All done."

let _ = main Sys.argv

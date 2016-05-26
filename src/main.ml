
let run_interactive model env checker =
  try
    while true do
      let line = read_line () in
      let (ide,points,qformula) = ModelLoader.load_ask_query env line in
      let res =  (Checker.qchecker (List.map model.Model.idspace points)
		    (model.Model.space#num_nodes) checker qformula)
      in
      Printf.printf "%s: %f\n%!" ide res
    done
  with End_of_file -> ()
      
let _ =
  let expfname =
    try
      Sys.argv.(1)
    with      
      Invalid_argument s ->
	Util.fail (Printf.sprintf "Usage: %s FILENAME\n" Sys.argv.(0))
  in
  Util.debug "Step 1/3: Loading experiment...";
  let (model,env,commands) = ModelLoader.load_experiment expfname in
  Model.load_cache model;
  (* TODO: check for output commands here! *)
  Util.debug "Step 2/3: Precomputing model checking table...";
  let t = Sys.time () in
  let checker = Checker.precompute model in  
  let products =
    List.fold_left
      (fun accum command ->
	match command with
	  ModelLoader.Ask (ide,ids,qformula) ->
	    Util.debug "Ask queries are supported only in server mode (see readme)"; []
	| ModelLoader.Check (color,formula) ->
	   (match accum with
	     [] -> Util.fail "no output file specified"
	   | (fname,fmlas)::rem ->
	      (fname,(color,(Util.toBool (checker formula)))::fmlas)::rem)
	| ModelLoader.Output (fname,states) ->
	   ((fname,states),[])::accum)
      [] commands 
  in
  let t' = (Sys.time ()) -. t in
  Util.debug (Printf.sprintf "Computation time (in seconds): %f" t');
  match products with
    [] ->
      Util.debug "Step 3/3: Interactive evaluation of Ask queries";
      run_interactive model env checker
  | _ ->
     begin
       Util.debug "Step 3/3: Writing output files...";
       List.iter (fun ((fname,states),(coloured_truth_vals)) -> model.Model.write_output fname states coloured_truth_vals) products;
       Model.save_cache model;
       Util.debug "All done."
     end

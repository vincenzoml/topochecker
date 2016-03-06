open Bigarray
       
let load_bmp_model dir k s e =
  if not (Filename.check_suffix s ".bmp") then None
  else
    Some (
	if k <> ""
	then Util.fail
	       "Support for temporal reasoning in bmp models is not yet implemented.";
	if e <> ""
	then Util.fail
	       "Atomic propositions in bmp models are embedded in the image and should not be specified in external files.";
	let f = Bmp.load_bmp (Util.mkfname dir s) in
	let len = Bytes.length f.Bmp.bmpBytes / 3 in
	let dims = [|f.Bmp.bmpInfoHeader.Bmp.biWidth;f.Bmp.bmpInfoHeader.Bmp.biHeight|] in	
	let h = Model.H.create 10 in
	let ch = Model.CH.create 10 in 
	let rgb = Array.init 3 (fun _ -> Array1.create float64 c_layout len) in
	for i = 0 to len - 1 do
	  for j = 0 to 2 do
	    Array1.set (Array.get rgb j) i (float_of_int (Char.code (Bytes.get f.Bmp.bmpBytes ((3*i)+j ))))
	  done
	done;
	List.iter (fun (idx,name) ->
		   Model.H.add h (Logic.Prop name) (fun k s -> Array1.get rgb.(idx) s))
		  [(0,"red");(1,"green");(2,"blue")];
	{ Model.kripke = Model.default_kripke ();
	  Model.collective_eval = ch;
	  Model.iter_ball =
	    Some
	      (fun center radius fn ->
	       let coords = Util.coords_of_int center dims in
	       Util.iter_hypercube dims coords radius
				   (fun point ->
				    if Util.in_range point dims &&
					 Util.euclidean_distance coords point <= radius
				    then fn (Util.int_of_coords point dims)));
	  Model.space =
	    { Model.num_nodes = len;
	      Model.iter_pre = (Util.iter_neighbour dims);
	      Model.iter_post = (Util.iter_neighbour dims)};
	  Model.eval = h;
	  Model.kripkeid = string_of_int;
	  Model.idkripke = int_of_string;
	  Model.idspace =
	    (fun id ->
	     Util.int_of_coords (Array.of_list
			      (List.map int_of_string
					(TcParser.stringlist
					   TcLexer.token (Lexing.from_string id))))
			   dims);
	  Model.spaceid =
	    (fun i ->
	     Printf.sprintf "(%s)"
			    (String.concat ","
					   (List.map string_of_int
						     (Array.to_list
							(Util.coords_of_int i dims)))));
	  deadlocks = None;
	  write_output = (fun filename _ coloured_truth_vals ->
			  let root = Util.mkfname dir filename in
			  let output = { f with Bmp.bmpBytes = Bytes.create (String.length f.Bmp.bmpBytes) } in
			  List.iter (fun (colour,truth) ->				     
				     for i = 0 to dims.(0) * dims.(1) - 1 do
				       let c = int_of_string colour in
				       let rgb = [|(c land 0xFF0000) lsr 16;(c land 0xFF00) lsr 8;c land 0xFF|] in
				       if truth 0 i then
					 for j = 0 to 2 do
					   Bytes.set output.Bmp.bmpBytes (i*3+j) (Char.chr rgb.(j))
					 done
				     done
				    ) coloured_truth_vals;
			  Bmp.save_bmp root output)			 
	})         

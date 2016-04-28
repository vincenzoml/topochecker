open Bigarray

let load_nifti_model dir k s e =
  if not (Filename.check_suffix k "#eca") then None
  else
    Some (
	let init = Bmp.load_bmp (Util.mkfname dir s) in
	
      let file = Filename.temp_file (Filename.basename s) "raw" in
      let header = Filename.temp_file (Filename.basename s) "header" in
      let call =  (Printf.sprintf "medcon -f \"%s\" -c - bin 1> \"%s\" 2> \"%s\"" (Util.mkfname dir s) file header) in
      let unixres = Unix.system call in
      match unixres with
	Unix.WEXITED 0 ->
	  (let (vect,header) = load_raw file header in
	   let h = Model.H.create 1 in
	   Model.H.add h (Logic.Prop "value") (fun k s -> float_of_int (Array1.get vect s));
	   { Model.kripke = Model.default_kripke ();
	     Model.space =
	       { Model.num_nodes = (Array1.dim vect);
		 Model.iter_pre = (Util.iter_neighbour Util.CityBlock header.dims);
		 Model.iter_post = (Util.iter_neighbour Util.CityBlock header.dims)};
	     Model.eval = h;
	     Model.kripkeid = string_of_int;
	     Model.idkripke = int_of_string;
	     Model.idspace =
	       (fun id ->
		 Util.int_of_coords (Array.of_list
				  (List.map int_of_string
				     (TcParser.stringlist
					TcLexer.token (Lexing.from_string id))))
		   header.dims);
	     Model.spaceid =
	       (fun i ->
		 Printf.sprintf "(%s)"
		   (String.concat ","
		      (List.map string_of_int
			 (Array.to_list
			    (Util.coords_of_int i header.dims)))));
	     deadlocks = None;
	     write_output = (fun filename _ coloured_truth_vals ->
	       let root = Util.mkfname dir filename in
	       let orig = Unix.openfile s [Unix.O_RDONLY] 0o644 in
	       let r = Unix.openfile
		 (Printf.sprintf "%s.nii" root)
		 [Unix.O_RDWR;Unix.O_CREAT;Unix.O_TRUNC] 0o644 in
	       let v1 = Array1.create header.valtype c_layout
		 (Array1.dim vect)
	       in
	       let v3 = Array1.map_file orig header.valtype c_layout false ~-1 in
	       let v2 = Array1.map_file r header.valtype c_layout true (Array1.dim v3) in

	       List.iter (fun (colour,truth) ->
		 for i = 0 to Array1.dim vect - 2 do
		   if truth 0 i then Array1.set v1 i (int_of_string colour)
		 done
	       ) coloured_truth_vals;
	       let delta = (Array1.dim v2) - (Array1.dim v1) in
	       Array1.blit (Array1.sub v3 0 delta) (Array1.sub v2 0 delta);
	       Array1.blit v1 (Array1.sub v2 delta (Array1.dim v1));
	       Unix.close r;
	       Unix.close orig
	     )
	   })         
      | _ -> Util.fail (Printf.sprintf "Error in conversion: %s" call))
      

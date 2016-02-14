open Bigarray

type endian = Big | Little

type ('a,'b) header =
  { dims : int array;
    valtype : ('a,'b) kind;
    endian : endian;
    full : string list }
  
let load_global_header fname =
  let h = Hashtbl.create 100 in
  let read k = Hashtbl.find h k in
  let f = open_in fname in
  let finished = ref false in
  let lines = ref [] in
  while not !finished do
    try
      let l = input_line f in
      lines := l :: !lines;
      if l = "-------------------------------------------------------------------------------"
      then finished := true	
      else
	try
	  let i = String.index l ':'  in
	  let k = String.trim (String.sub l 0 (i-1)) in
	  let v = String.trim (String.sub l (i+1) (String.length l - i - 1)) in	
	  let v' =
	    try
	      let j = max 0 (String.index v '(' - 1) in
	      (String.trim (String.sub v 0 j))
	    with Not_found -> v
	  in
	  Hashtbl.add h k v'
	with Not_found -> ()
    with End_of_file -> finished := true
  done;
  (try
     while true do
       lines := (input_line f)::!lines
     done
   with End_of_file -> ());
  close_in f;
  let dim = int_of_string (read "dim[0]") in
  let dims = Array.create dim 0 in
  for i = 0 to dim - 1 do
    dims.(i) <- int_of_string (read (Printf.sprintf "dim[%d]" (i+1)))
  done;
  { dims = dims;
    valtype = (match read "type" with "4" -> int16_unsigned | _ -> Util.fail "Value type not supported in nifti");
    endian = (match read "endian" with "1" -> Little | _ -> Util.fail "Big endian not supported in nifti");
    full = List.rev !lines }
   
let int_of_coords coords dims =
  let (r,_,_) = Array.fold_left (fun (acc,fac,dim) coord -> (coord * fac + acc,dims.(dim)*fac,dim+1)) (0,1,0) coords in
  r

let coords_of_int i dims =
  let products = Array.create ((Array.length dims) - 1) dims.(0) in
  for i = 1 to Array.length products - 1 do
    products.(i) <- dims.(i) * products.(i-1)
  done;
  let res = Array.create (Array.length dims) 0 in
  let r = ref i in
  let q = ref 0 in
  let j = ref 0 in
  while !j <= Array.length products - 1 do
    let k = Array.length products - !j - 1 in
    let v = !r in
    q := v / products.(k);
    r := v mod products.(k);
    res.(k+1) <- !q;
    j := !j + 1;    
  done;
  res.(0) <- !r;
  res
    
    
let load_raw rawfile header =
  let header = load_global_header header in
  let vect = Array1.map_file (Unix.openfile rawfile [Unix.O_RDONLY] 0o644) header.valtype c_layout false ~-1 in
  (vect,header)

let iter_neighbour dims i fn =
  let coords = coords_of_int i dims in
  for i = 0 to Array.length coords - 1 do
    let (orig,x,y) = (coords.(i),coords.(i) - 1,coords.(i) + 1) in
    if x >= 0 then 
      begin
	coords.(i) <- x;
	fn (int_of_coords coords dims);
      end;
    if y < dims.(i) then
      begin
	coords.(i) <- y;
	let id = int_of_coords coords dims in
	fn id;
      end;
    coords.(i) <- orig
  done
    
let load_nifti_model dir k s e =
  if not (Filename.check_suffix s ".nii") then None
  else
    Some (
      if k <> "" then Util.fail
	"Support for temporal reasoning in nifti models is not yet implemented.";
      if e <> "" then Util.fail
	"Atomic propositions in nifti models are embedded in the image and should not be specified in external files.";
      let file = Filename.temp_file (Filename.basename s) "raw" in
      let header = Filename.temp_file (Filename.basename s) "header" in
      let call =  (Printf.sprintf "medcon -f \"%s\" -c - bin 1> \"%s\" 2> \"%s\"" (Util.mkfname dir s) file header) in
      let unixres = BatUnix.system call in
      match unixres with
	BatUnix.WEXITED 0 ->
	  (let (vect,header) = load_raw file header in
	   let h = Model.H.create 1 in
	   Model.H.add h (Logic.Prop "value") (fun k s -> float_of_int (Array1.get vect s));
	   { Model.kripke = Model.default_kripke ();
	     Model.space =
	       { Model.num_nodes = (Array1.dim vect);
		 Model.iter_pre = (iter_neighbour header.dims);
		 Model.iter_post = (iter_neighbour header.dims)};
	     Model.eval = h;
	     Model.kripkeid = string_of_int;
	     Model.idkripke = int_of_string;
	     Model.idspace =
	       (fun id ->
		 int_of_coords (Array.of_list
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
			    (coords_of_int i header.dims)))));
	     deadlocks = None;
	     write_output = (fun filename _ coloured_truth_vals ->
	       let root = Util.mkfname dir filename in
	       (* let h = open_out (Printf.sprintf "%s.hdr" root)  in
	       	  List.iter (fun s -> Printf.fprintf h "%s\n%!" s) 
		  header.full; 
		  close_out h; *)
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
      

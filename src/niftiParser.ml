open Bigarray

type endian = Big | Little

type header =
  { dims : int array;
    pixdims : float array;
    endian : endian;
    full_header : string list;
    dim : int;
    slice : int -> float }

class spaceNifti numnodes dims pixdims=
object
  inherit Util.euclidean_grid
  method num_nodes = numnodes
  method iter_pre = Util.iter_neighbour Util.Euclidean dims pixdims
  method iter_post =Util.iter_neighbour Util.Euclidean dims pixdims
  method dims = dims
  method pixdims = pixdims
  method euclidean_distance = fun p1 p2 ->
    let v1 = Util.coords_of_int p1 dims in
    let v2 = Util.coords_of_int p2 dims in
    Util.euclidean_distance v1 v2 pixdims
end

let load_raw rawfile header =
  let h = Hashtbl.create 100 in
  let read k = Hashtbl.find h k in
  let f = open_in header in
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
  let dim =
    try
      int_of_string (read "dim[0]")
    with
      a -> 2 (*TODO:check if always true*)
  in
  let dims = Array.make dim 0 in
  for i = 0 to dim - 1 do
    dims.(i) <- int_of_string (read (Printf.sprintf "dim[%d]" (i+1)))
  done;
  let pixdims = Array.make dim 0.0 in
  for i = 0 to dim - 1 do
    let tmp=read (Printf.sprintf "pixdim[%d]" (i+1)) in
    let tmp2=String.sub tmp 0 (String.index tmp '[' - 1) in
    pixdims.(i) <- float_of_string tmp2;
  done;
  let vect valtype = Array1.map_file (Unix.openfile rawfile [Unix.O_RDONLY] 0o644) valtype c_layout false ~-1 in
  let datatype = read "type" in
  let (slice,dim) =
    match datatype with
    | "1" (*binary*) -> let v = vect int8_unsigned in ((fun i -> float_of_int (Array1.get v i)),Array1.dim v)
    | "2" -> let v = vect int8_unsigned in ((fun i -> float_of_int (Array1.get v i)),Array1.dim v)
    | "4" -> let v = vect int16_signed in ((fun i -> float_of_int (Array1.get v i)),Array1.dim v)
    | "8" -> let v = vect int32 in ((fun i -> Int32.to_float (Array1.get v i)),Array1.dim v)
    | "16" -> let v = vect float32 in ((fun i -> Array1.get v i),Array1.dim v)
    | "32" (* complex32 2 x float 16 *) -> Util.fail "Complex 32 type not supported in nifti"
    | "64" -> let v = vect float64 in ((fun i -> Array1.get v i),Array1.dim v)
    | "128" (* rgb 3 x int8 *) ->  Util.fail "RGB type not supported in nifti"
    | "256" -> let v = vect int8_signed in ((fun i -> float_of_int (Array1.get v i)) ,Array1.dim v)
    | "512" -> let v = vect int16_unsigned in ((fun i -> float_of_int (Array1.get v i)) ,Array1.dim v)
    | "768" (* int32 (*unsigned*) *) -> Util.fail "Unsigned integer 32 type not supported in nifti"
    | "1024" -> let v = vect int64 in ((fun i -> Int64.to_float (Array1.get v i)),Array1.dim v)
    | "1280" (* int64 (*unsigned*) *) ->  Util.fail "Unsigned integer 64 type not supported in nifti"
    | "1536" (* float128 *) ->  Util.fail "Float 128 type not supported in nifti"
    | "1792" (* complex64 2 x float64*) -> Util.fail "Complex 64 type not supported in nifti"
    | "2048" (* complex256 2 x float128 *) -> Util.fail "Complex 256 type not supported in nifti"
    | _ -> Util.fail "Unknown value type in nifti"
  in
  { dims = dims;
    pixdims = pixdims;
    endian = (match read "endian" with "1" -> Little | _ -> Util.fail "Big endian not supported in nifti");
    full_header = List.rev !lines;
    dim = dim;
    slice = slice }

let load_nifti s =
  let file = Filename.temp_file (Filename.basename s) "raw" in
  let header = Filename.temp_file (Filename.basename s) "header" in
  let call =  (Printf.sprintf "medcon -f \"%s\" -c - bin 1> \"%s\" 2> \"%s\"" s file header) in
  let unixres = Unix.system call in
  let error =
    match unixres with
      Unix.WEXITED 0 ->
      None
    | Unix.WEXITED i -> Some (Printf.sprintf "terminated with code %d" i)
    | Unix.WSIGNALED i -> Some (Printf.sprintf "killed with signal %d" i)
    | Unix.WSTOPPED i -> Some (Printf.sprintf "stopped with signal %d" i)
  in
  match error with
    None ->   load_raw file header;
  | Some s -> Util.fail (Printf.sprintf "Error while loading nifti. Medcon %s" s)

(*dir: directory, s=file name, k,e=""*)
let load_nifti_model bindings =
  (let prop_img = List.map (fun (name,file) -> ((match name with "" -> "value" | s -> s),load_nifti file)) bindings in
   
   let hash = Util.mapO (fun x -> (x,Model.H.create 1)) (Util.sfsSha256 bindings) in
   let (_,origfname) = List.hd bindings in
   let (prop,main) = List.hd prop_img in
   let dims = main.dims in
   let pixdims = main.pixdims in
   let h = Model.H.create 1 in
   (* TODO: check that all the images have the same dimensions *)
   List.iter (fun (prop,img) -> 
	      Model.H.add h (Logic.Prop prop)
		(fun k s -> img.slice s))
     prop_img;
   { Model.hash_and_cache = hash;
     Model.kripke = Model.default_kripke ();
     (*distance 1 not euclidean*)
     Model.iter_ball =
       Some
	 (fun center radius fn ->
	  let coords = Util.coords_of_int center dims in
	  Util.iter_hypercube_w dims pixdims coords radius
	    (fun point ->
	      if Util.in_range point dims
	      then fn (Util.int_of_coords point dims)));
     Model.euclidean_distance =
       Some
	 (fun p1 p2 ->
	   let v1 = Util.coords_of_int p1 dims in
	   let v2 = Util.coords_of_int p2 dims in
	   Util.euclidean_distance v1 v2 pixdims
	 );
     Model.space = new spaceNifti main.dim dims pixdims;
     Model.eval = h;
     Model.kripkeid = string_of_int;
     Model.idkripke = int_of_string;
     Model.idspace =
       (fun id ->
	Util.int_of_coords (Array.of_list
			      (List.map int_of_string
					(TcParser.stringlist
					   TcLexer.token (Lexing.from_string id))))
			   main.dims);
     Model.spaceid =
       (fun i ->
	Printf.sprintf "(%s)"
		       (String.concat ","
				      (List.map string_of_int
						(Array.to_list
						   (Util.coords_of_int i main.dims)))));
     Model.deadlocks = None;
     Model.write_output = (fun filename _ coloured_truth_vals ->       
       let orig = Unix.openfile origfname [Unix.O_RDONLY] 0o644 in
       let r = Unix.openfile filename
	 [Unix.O_RDWR;Unix.O_CREAT;Unix.O_TRUNC] 0o644 in
       let valtype = int16_signed in
       let v1 = Array1.create valtype c_layout main.dim
       in
       let v3 = Array1.map_file orig valtype c_layout false ~-1 in
       let v2 = Array1.map_file r valtype c_layout true (Array1.dim v3) in
       
       List.iter (fun (colour,truth) ->
	 for i = 0 to main.dim - 1 do
	   if truth 0 i then Array1.set v1 i (int_of_string colour)
	 done
       ) coloured_truth_vals;
       let delta = (Array1.dim v2) - (Array1.dim v1) in
			     Array1.blit (Array1.sub v3 0 delta) (Array1.sub v2 0 delta);
       Array1.blit v1 (Array1.sub v2 delta (Array1.dim v1));
       Unix.close r;
       Unix.close orig)})
    
    

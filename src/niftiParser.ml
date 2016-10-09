open Bigarray

type endian = Big | Little
type nifti_ver = NIFTI1 | NIFTI2

type header = {
  version: nifti_ver;
  datatype : int;
  dims : int array;
  pixdims : float array;
  endian : endian;
  full_header : (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t;
  dim : int;
  data : (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t}

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

let from_bytes_to_int buf endian =
  let rec build_int id nacc =
    let pow =
      match endian with
      | Little -> float_of_int id
      | Big -> float_of_int ((Array1.dim buf) - 1 - id)
    in
    if id<Array1.dim buf then
      build_int (id+1) (nacc+.(float_of_int(buf.{id}) *. (2.0 ** (8.0 *. pow))))
    else
      int_of_float nacc
  in build_int 0 0.0

let load_head_ver nii header version =
  let ndimsbytes =
    match version with
    | NIFTI1 -> Array1.sub header 40 2
    | NIFTI2 -> Array1.sub header 16 8
  in
  let ndimsMtmp = from_bytes_to_int ndimsbytes Little in
  let endian = if (ndimsMtmp>=1 && ndimsMtmp<8) then Little else Big in
  let ndimsM = from_bytes_to_int ndimsbytes endian in
  let voffs =
    match version with
    | NIFTI1 -> Int64.of_float (Int32.float_of_bits (Int32.of_int (from_bytes_to_int (Array1.sub header 108 4) endian)))
    | NIFTI2 -> Int64.of_int (from_bytes_to_int (Array1.sub header 168 8) endian) (*TODO: on 32 bit platform not recognized (>4GB)*)
  in
  let datatype =
    match version with
    | NIFTI1 -> from_bytes_to_int (Array1.sub header 70 2) endian
    | NIFTI2 -> from_bytes_to_int (Array1.sub header 12 2) endian
  in
  let dimsM = Array.make ndimsM 0 in
  for n=0 to (ndimsM-1) do
    dimsM.(n) <-
      match version with
      | NIFTI1 -> from_bytes_to_int (Array1.sub header (42+2*n) 2) endian
      | NIFTI2 -> from_bytes_to_int (Array1.sub header (24+8*n) 8) endian
  done;
  let dims = Array.of_list (List.filter (fun x -> x!=1) (Array.to_list dimsM)) in
  let ndims = Array.length dims in
  let dim =
    let rec prod acc id =
      match id with
      | 0 -> acc*dims.(0)
      | _ -> prod (acc*dims.(id)) (id-1)
    in
    prod 1 (ndims-1) in
  let pixdims = Array.make ndims 0.0 in
  for n=0 to (ndims-1) do
    pixdims.(n) <-
      match version with
      | NIFTI1 -> Int32.float_of_bits (Int32.of_int (from_bytes_to_int (Array1.sub header (80+4*n) 4) endian))
      | NIFTI2 -> Int64.float_of_bits (Int64.of_int (from_bytes_to_int (Array1.sub header (112+8*n) 8) endian))
  done;
  let (scl_slope,scl_off) =
    match version with
    | NIFTI1 ->
       let slope=Int32.float_of_bits (Int32.of_int (from_bytes_to_int (Array1.sub header 112 4) endian)) in
       let scaldata = Int32.float_of_bits (Int32.of_int (from_bytes_to_int (Array1.sub header 116 4) endian)) in
       (slope,scaldata)
    | NIFTI2 ->
       let slope=Int64.float_of_bits (Int64.of_int (from_bytes_to_int (Array1.sub header 176 8) endian)) in
       let scaldata = Int64.float_of_bits (Int64.of_int (from_bytes_to_int (Array1.sub header 184 8) endian)) in
       (slope,scaldata)
  in
  let scaleData level = scl_off +. (level *. scl_slope) in
  let vect valtype = Array1.map_file nii ?pos:(Some voffs) valtype c_layout false ~-1 in
  let data = Array1.create float64 c_layout dim in
  (match datatype with
  | 1 (*binary*) -> Util.fail "Binary type not supported in nifti"
  | 2 -> let v = vect int8_unsigned in
	 for n=0 to (dim-1) do
	   let vox_lev=float_of_int v.{n} in
	   data.{n}<-scaleData vox_lev;
	 done
  | 4 -> let v = vect int16_signed in
	 for n=0 to (dim-1) do
	   let vox_lev=float_of_int v.{n} in
	   data.{n}<-scaleData vox_lev;
	 done
  | 8 -> let v = vect int32 in
	 for n=0 to (dim-1) do
	   let vox_lev=Int32.to_float v.{n} in
	   data.{n}<-scaleData vox_lev;
	 done
  | 16 -> let v = vect float32 in
	  for n=0 to (dim-1) do
	    let vox_lev=v.{n} in
	    data.{n}<-scaleData vox_lev;
	  done
  | 32 (* complex32 2 x float 16 *) -> Util.fail "Complex 32 type not supported in nifti"
  | 64 -> let v = vect float64 in
	  for n=0 to (dim-1) do
	    let vox_lev=v.{n} in
	    data.{n}<-scaleData vox_lev;
	  done
  | 128 (* rgb 3 x int8 *) ->  Util.fail "RGB type not supported in nifti"
  | 256 -> let v = vect int8_signed in
	   for n=0 to (dim-1) do
	     let vox_lev=float_of_int v.{n} in
	     data.{n}<-scaleData vox_lev;
	   done
  | 512 -> let v = vect int16_unsigned in
	   for n=0 to (dim-1) do
	     let vox_lev=float_of_int v.{n} in
	     data.{n}<-scaleData vox_lev;
	   done
  | 768 (* int32 (*unsigned*) *) -> Util.fail "Unsigned integer 32 type not supported in nifti"
  | 1024 -> let v = vect int64 in
	    for n=0 to (dim-1) do
	      let vox_lev=Int64.to_float v.{n} in
	      data.{n}<-scaleData vox_lev;
	    done
  | 1280 (* int64 (*unsigned*) *) ->  Util.fail "Unsigned integer 64 type not supported in nifti"
  | 1536 (* float128 *) ->  Util.fail "Float 128 type not supported in nifti"
  | 1792 (* complex64 2 x float64*) -> Util.fail "Complex 64 type not supported in nifti"
  | 2048 (* complex256 2 x float128 *) -> Util.fail "Complex 256 type not supported in nifti"
  | _ -> Util.fail "Unknown value type in nifti");
  {
    version=version;
    datatype = datatype;
    dims = dims;
    pixdims = pixdims;
    endian = endian;
    full_header=header;
    dim=dim;
    data=data;
  }

let load_nifti2 s =
  let f = Unix.openfile s [Unix.O_RDONLY] 0o644 in
  let hbytes = Array1.map_file f int8_unsigned c_layout false 540 in
  let hsizeL = from_bytes_to_int (Array1.sub hbytes 0 4) Little in
  let hsizeB = from_bytes_to_int (Array1.sub hbytes 0 4) Big in
  let (hsize,versionOpt) =
    if hsizeL == 348 || hsizeB == 348 then (348,Some (NIFTI1))
    else if hsizeL == 540 || hsizeB == 540 then (540,Some (NIFTI2))
    else (-1,None)
  in match versionOpt with
  | None -> Util.fail "File format unknown";
  | Some version ->
     let full_header = Array1.sub hbytes 0 hsize in
     load_head_ver f full_header version

(*dir: directory, s=file name, k,e=""*)
let load_nifti_model bindings =
  (let prop_img = List.map (fun (name,file) -> ((match name with "" -> "value" | s -> s),load_nifti2 file)) bindings in
   
   let hash = Util.mapO (fun x -> (x,Model.H.create 1)) (Util.sfsSha256 bindings) in
   let (_,origfname) = List.hd bindings in
   let (prop,main) = List.hd prop_img in
   let dims = main.dims in
   let pixdims = main.pixdims in
   let h = Model.H.create 1 in
   (* TODO: check that all the images have the same dimensions *)
   List.iter (fun (prop,img) -> 
	      Model.H.add h (Logic.Prop prop)
		(fun k s -> img.data.{s}))
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
       let valtype = int16_signed in
       let hsize=Array1.dim main.full_header in
       let offs =
	 match main.version with
	 | NIFTI1 -> 4
	 | NIFTI2 -> 0
       in
       let headerOut = Array1.create int8_unsigned c_layout (Array1.dim main.full_header) in
       Array1.blit main.full_header headerOut;
       let datatypeOut=4 in
       let bitpixOut=16 in
       let intentOut=[|234;3|] in
       let dataoffs = (hsize+offs)/(bitpixOut/8) in
       (match main.version with
       | NIFTI1 ->
	 (match main.endian with
	 | Little ->
	   headerOut.{70} <- datatypeOut;
	   headerOut.{71} <- 0;
	   headerOut.{72} <- bitpixOut;
	   headerOut.{73} <- 0;
	   (*vox offset*)
	   headerOut.{108} <- 0;
	   headerOut.{109} <- 0;
	   headerOut.{110} <- 176;
	   headerOut.{111} <- 67;
	   (*data scaling*)
	   headerOut.{112} <- 0;
	   headerOut.{113} <- 0;
	   headerOut.{114} <- 128;
	   headerOut.{115} <- 63;
	   headerOut.{116} <- 0;
	   headerOut.{117} <- 0;
	   headerOut.{118} <- 0;
	   headerOut.{119} <- 0;
	   (*intent Label*)
	   headerOut.{68}<-intentOut.(0);
	   headerOut.{69}<-intentOut.(1);
	 | Big ->
	   headerOut.{71} <- datatypeOut;
	   headerOut.{70} <- 0;
	   headerOut.{73} <- bitpixOut;
	   headerOut.{72} <- 0;
	   (*vox offset*)
	   headerOut.{111} <- 0;
	   headerOut.{110} <- 0;
	   headerOut.{109} <- 176;
	   headerOut.{108} <- 67;
	   (*data scaling*)
	   headerOut.{115} <- 0;
	   headerOut.{114} <- 0;
	   headerOut.{113} <- 128;
	   headerOut.{112} <- 63;
	   headerOut.{119} <- 0;
	   headerOut.{118} <- 0;
	   headerOut.{117} <- 0;
	   headerOut.{116} <- 0;
	   (*intent Label*)
	   headerOut.{69}<-intentOut.(0);
	   headerOut.{68}<-intentOut.(1);
	 )
       | NIFTI2 ->
	 (*TODO: data scaling*)
	 (*vox offset*)
	 headerOut.{168} <- 0;
	 headerOut.{169} <- 0;
	 headerOut.{170} <- 0;
	 headerOut.{171} <- 0;
	 headerOut.{172} <- 0;
	 headerOut.{173} <- 0;
	 headerOut.{174} <- 0;
	 headerOut.{175} <- 0;
	 (match main.endian with
	 | Little ->
	   headerOut.{12} <- datatypeOut;
	   headerOut.{13} <- 0;
	   headerOut.{14} <- bitpixOut;
	   headerOut.{15} <- 0;
	   (*intent Label*)
	   headerOut.{504}<-intentOut.(0);
	   headerOut.{505}<-intentOut.(1);
	   headerOut.{506}<-0;
	   headerOut.{507}<-0;
	 | Big ->
	   headerOut.{13} <- datatypeOut;
	   headerOut.{12} <- 0;
	   headerOut.{15} <- bitpixOut;
	   headerOut.{14} <- 0;
	   (*intent Label*)
	   headerOut.{507}<-intentOut.(0);
	   headerOut.{506}<-intentOut.(1);
	   headerOut.{505}<-0;
	   headerOut.{504}<-0;
	 ););

       let v1 = Array1.create valtype c_layout main.dim in
       let r = Unix.openfile filename [Unix.O_RDWR;Unix.O_CREAT;Unix.O_TRUNC] 0o644 in
       let v2 = Array1.map_file r int8_unsigned c_layout true ((Array1.dim v1)*(bitpixOut/8)+hsize+offs) in
       Array1.blit headerOut (Array1.sub v2 0 hsize);
       Unix.close r;
       List.iter (fun (colour,truth) ->
       	 for i = 0 to main.dim - 1 do
       	   if truth 0 i then Array1.set v1 i (int_of_string colour)
       	 done
       ) coloured_truth_vals;
       let r = Unix.openfile filename [Unix.O_RDWR] 0o644 in
       let v2 = Array1.map_file r valtype c_layout true ~-1 in
       Array1.blit v1 (Array1.sub v2 dataoffs (Array1.dim v1));
       Unix.close r)})

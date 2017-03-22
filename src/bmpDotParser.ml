open Graph.Dot_ast
open Bigarray
open Images
open Rgb24

let load_image filename =
  match Bmp.load filename [] with
  | Rgb24 rgbimg -> rgbimg
  |  _ -> failwith "Only RGB24 bmp images supported at the moment."

let save_image filename img =
  Bmp.save filename [] (Rgb24 img)

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

let  write_state images state coloured_truth_vals out_name =
  let newimg = Rgb24.copy images.(state) in
  List.iter (fun (colour,tbl) ->
    let dims = [|images.(state).width;images.(state).height|] in
    for i = 0 to dims.(0) - 1 do
      for j = 0 to dims.(1) - 1 do
	if tbl state ((j * dims.(0)) + i) then Rgb24.set newimg i j (Color.color_parse colour)
      done
    done
  ) coloured_truth_vals;
  save_image out_name newimg
  
let write_dot_model kripkegraph images kripkeid fname states colored_truth_vals =
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
       let out_name =  (Printf.sprintf "%s_%s.bmp" fname (kripkeid state)) in
       let output = open_out out_name in
       write_state images state (List.rev colored_truth_vals) out_name;
       close_out output
     in
     aux fn
  
class spaceBmp width height =
  let dims = [|width;height|] in
  let pixdims = [|1.0;1.0|] in
object
  inherit TcUtil.euclidean_grid
  method num_nodes = width * height
  method dims : int array  = dims
  method pixdims : float array = pixdims
  method iter_pre = TcUtil.iter_neighbour TcUtil.Euclidean dims pixdims
  method iter_post = TcUtil.iter_neighbour TcUtil.Euclidean dims pixdims
  method euclidean_distance = fun p1 p2 ->
    let v1 = TcUtil.coords_of_int p1 dims in
    let v2 = TcUtil.coords_of_int p2 dims in
    TcUtil.euclidean_distance v1 v2 pixdims
end
     
let load_bmpdot_model bindings =
  let (kripkef,spacef) =
    try
      if List.length bindings <> 2 then raise Not_found
      else (List.assoc "KRIPKE" bindings,List.assoc "SPACE" bindings)
    with Not_found -> TcUtil.fail "Wrong model specification for bmpdot model. Format is 'bmpdot:kripke=<filename>,space=<filename>'"
  in
    
  (*  let kripkef = TcUtil.mkfname dir k in *)
  let (kripke,(k_id_of_int,k_int_of_id)) =
    if kripkef = ""
    then (Model.default_kripke (),(string_of_int,int_of_string))
    else (Parser.parse kripkef,ParserSig.read ())
  in
  let numstates = Model.Graph.nb_vertex kripke in
  let images = Array.init numstates (fun i ->  load_image (Printf.sprintf "%s_%d.bmp" spacef i)) in
  (* TODO: can we hash all the bmp files? Could we hash their modtime instead for efficiency? let hash = TcUtil.fsSha256 [kripkef;spacef;evalf] in *)
  (* TODO: check that all images have the same size *)
  let width = images.(0).width in
  let height = images.(0).height in
  let hash = None in

  let space = new spaceBmp width height in 
      
  let eval = Model.H.create 3 in
  Model.H.add eval (Logic.Prop "red") (fun state point -> let [|x;y|] = TcUtil.coords_of_int point space#dims in float_of_int (Rgb24.get images.(state) x y).r);
  Model.H.add eval (Logic.Prop "green") (fun state point -> let [|x;y|] = TcUtil.coords_of_int point space#dims in float_of_int (Rgb24.get images.(state) x y).g);
  Model.H.add eval (Logic.Prop "blue") (fun state point -> let [|x;y|] = TcUtil.coords_of_int point space#dims in float_of_int (Rgb24.get images.(state) x y).b);
  
  { Model.kripke = kripke;
    Model.hash_and_cache = TcUtil.mapO (fun x -> (x,Model.H.create 1)) hash;
    Model.space = space;
    Model.deadlocks = None;
    Model.iter_ball = None;
    Model.euclidean_distance = None;
    Model.write_output = write_dot_model kripke images k_id_of_int;
    (* (fun i -> TcUtil.int_of_coords i space#dims); *)
    (* (fun prefix opt_states coloured_truth_values -> write_dotbmp_model images prefix opt_states coloured_truth_values); *)
    Model.kripkeid = k_id_of_int;
    Model.idkripke = k_int_of_id;
    Model.idspace =
      (fun id ->
	TcUtil.int_of_coords (Array.of_list
			      (List.map int_of_string
				 (TcParser.stringlist
				    TcLexer.token (Lexing.from_string id))))
	  space#dims);
    Model.spaceid =
      (fun i ->
	Printf.sprintf "(%s)"
	  (String.concat ","
	     (List.map string_of_int
		(Array.to_list
		   (TcUtil.coords_of_int i space#dims)))));
    Model.eval = eval;	
  }
    

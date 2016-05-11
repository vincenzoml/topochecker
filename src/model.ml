open Logic
open Bigarray
open Slice

module H =
  Hashtbl.Make(
    struct
      type t = Logic.formula
      let equal = (=)
      let hash = Hashtbl.hash
    end)
    
module Vertex =
  struct
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

module Edge =
  struct
    type t = float
    let compare = compare
    let default = 1.0
  end
    
module Graph = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled
  (Vertex)(Edge)
  
type model =
  { hash_and_cache : (string * slice H.t) option; (* enables caching *)
    kripke : Graph.t;
    space : Util.simple_graph;
    deadlocks : (int -> float) option;
    kripkeid : int -> string;
    idkripke : string -> int;
    spaceid : int -> string;
    idspace : string -> int;
    iter_ball : (int -> float -> (int -> unit) -> unit) option;
    euclidean_distance : (int -> int -> float) option;
    write_output : string -> (int list option) -> (string * (int -> int -> bool)) list -> unit; (* filename -> optional list of states -> list of pairs colour,truth table *)
    eval : (int -> int -> float) H.t;
  }

let save_cache model =
  match model.hash_and_cache with
    None -> ()
  | Some (model_hash,cache) ->
     H.iter
       (fun formula slice ->
	 let formula_repr = Marshal.to_string formula [] in
	 let formula_hash = Util.sha256 formula_repr in
 (*	 let formula_fname = Printf.sprintf "%s_%s_%s.fmla" model_hash formula_hash (Logic.string_of_formula formula) *)
	 let formula_fname = Printf.sprintf "%s_%s.fmla" model_hash formula_hash in 
	 if not (Sys.file_exists formula_fname) then
	   begin
	     let formula_chan = open_out_bin formula_fname in
	     output_string formula_chan formula_repr;
	     Printf.fprintf formula_chan "\n%s\n" (Logic.string_of_formula formula);
	     close_out formula_chan;
	     let slice_fname = Printf.sprintf "%s_%s.slice" model_hash formula_hash in
	     save_slice slice slice_fname
	   end)
       cache

let load_cache model =
  match model.hash_and_cache with
    None -> () 
  | Some (model_hash,cache) ->
      let v = Sys.readdir "." in
      for i = 0 to Array.length v - 1 do
	let found_formula_fname = v.(i) in
	if Filename.check_suffix found_formula_fname ".fmla" then
	  let found_model_hash = String.sub v.(i) 0 64 in
	  if found_model_hash = model_hash then
	    let found_formula_hash = String.sub v.(i) 65 64 in
	    let slice_fname = Printf.sprintf "%s_%s.slice" model_hash found_formula_hash in
	    if Sys.file_exists slice_fname then
	      let formula_chan = open_in found_formula_fname in
	      let formula = Marshal.from_channel formula_chan in
	      close_in formula_chan;
	      if not (H.mem model.eval formula) then
		let slice = load_slice slice_fname (Graph.nb_vertex model.kripke) (model.space.Util.num_nodes) in
		H.replace cache formula slice;
		H.replace model.eval formula (Array2.unsafe_get slice);
      done
	
let default_kripke () =
  let g = Graph.create () in
  Graph.add_vertex g 0; 
  g
 
let completeDeadlocks model =
  let found = ref None in
  Graph.iter_vertex
    (fun v -> if 0 = Graph.out_degree model.kripke v
      then begin
	let vect =
	  match !found with
	    None ->
	      let a = Array1.create float64 c_layout
		(Graph.nb_vertex model.kripke)
	      in
	      Array1.fill a Util.valFalse;
	      found := Some a;
	      a
	  | Some a -> a
	in
	Graph.add_edge model.kripke v v;
	Array1.set vect v Util.valTrue
      end)
    model.kripke;
  match !found with
    None -> model
  | Some vect ->
     { model with
       deadlocks = (Some (fun state -> Array1.get vect state)) }

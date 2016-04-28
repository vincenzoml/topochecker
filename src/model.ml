open Logic
open Bigarray

module H = Hashtbl.Make(
	       struct
		 type t = Logic.formula
		 let equal = (=)
		 let hash = Hashtbl.hash
	       end)					  

module CH = Hashtbl.Make (
		struct
		  type t = Logic.cformula
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
  { kripke : Graph.t;
    space : Util.simple_graph;
    deadlocks : (int -> bool) option;
    kripkeid : int -> string;
    idkripke : string -> int;
    spaceid : int -> string;
    idspace : string -> int;
    iter_ball : (int -> float -> (int -> unit) -> unit) option;
    euclidean_distance : (int -> int -> float) option;
    write_output : string -> (int list option) -> (string * (int -> int -> bool)) list -> unit; (* filename -> optional list of states -> list of pairs colour,truth table *)
    eval : (int -> int -> float) H.t;
    collective_eval : (int -> float) CH.t
  }
      
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
       deadlocks = (Some (fun state -> Util.isTrue (Array1.get vect state))) }

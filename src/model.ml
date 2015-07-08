open Logic
open Bigarray

type slice = (float,float64_elt,c_layout) Bigarray.Array2.t


module H = Hashtbl.Make(
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
    
module Graph = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(Vertex)(Edge)
			       
type 'prop model =
    { kripke : Graph.t;
      space : Graph.t;
      kripkeid : int -> string;
      spaceid : int -> string;
      eval : slice H.t }

      

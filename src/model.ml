open Logic

module Vertex =
  struct
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end
    
module Graph = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
			       
type 'prop model =
    { kripke : Graph.t;
      space : Graph.t;
      eval : (formula,Slice.t) Hashtbl.t}

      

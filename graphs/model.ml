open Logic

module Vertex =
  struct
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = (=)
  end
    
module Space = Graph.Imperative.Digraph.ConcreteBidirectional(Vertex)
			       
type 'prop model =
    { kripke : Space.t;
      space : Space.t;
      eval : (formula,Slice.t) Hashtbl.t}

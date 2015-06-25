open Logic
open Model
open Bigarray
open IntBool		      
       
let precompute model =
    let num_states = Graph.nb_vertex model.kripke in
    let num_points = Graph.nb_vertex model.space in
    let rec cache f =
      try
	Hashtbl.find model.eval f 
      with Not_found ->
	let slice = Array2.create int c_layout num_states num_points in
	Hashtbl.add model.eval f slice;
	let iter fn = for state = 0 to num_states - 1 do
			for point = 0 to num_points - 1 do
			  Array2.set slice state point (fn state point)
			done
		      done in
	(match f with 
	   T -> Array2.fill slice valTrue
	 | Prop p -> (* TODO check whether p really is in eval? *) ()
	 | Not f1 -> let a1 = cache f1 in
		     iter (fun state point -> valNot (Array2.get a1 state point))
	 | And (f1,f2) -> let a1 = cache f1 in
			  let a2 = cache f2 in
			  iter (fun state point -> valAnd (Array2.get a1 state point) (Array2.get a2 state point))
	 | Near f1 -> let a1 = cache f1 in
		      for state = 0 to num_states - 1 do
			Graph.iter_vertex (fun point ->
					   if isTrue (Array2.get a1 state point) then					     
					     (Array2.set slice state point valTrue;
					      Graph.iter_succ (fun point -> Array2.set slice state point valTrue) model.space point)
					   else Array2.set slice state point valFalse)
					  model.space
		      done
	 | Surrounded (f1,f2) ->
	    Array2.fill slice 0;
	    let a1 = cache f1 in
            let a2 = cache f2 in
            let accum = ref [] in
	    for state = 0 to num_states - 1 do
              for point = 0 to num_points - 1 do
		Array2.set slice state point (Array2.get a1 state point);
		if isTrue (Array2.get a1 state point) || isTrue (Array2.get a2 state point) then
		  Model.Graph.iter_succ
		    (fun point -> if (isFalse (Array2.get a1 state point)) &&
				       (isFalse(Array2.get a2 state point)) &&
					 (isFalse(Array2.get slice state point))
				  then (Array2.set slice state point valUtil; accum := point::!accum))
		    model.space point
	      done;
	    while !accum != [] do
	      match !accum with
		[] -> ()
	      | point::points ->
		 Array2.set slice state point valFalse;
		 Model.Graph.iter_pred (fun point ->						       
					if isTrue (Array2.get slice state point) then
					  begin
					    Array2.set slice state point valFalse;
					    if isFalse (Array2.get a2 state point) then accum := point :: points
					  end
				       ) model.space point
	    done;
	    done;
	 | Af f1 ->
	    Util.fail "Af not supported in model checker yet"
	 | Ex f1 ->
	    Array2.fill slice valFalse;
	    let a1 = cache f1 in
	    for state = 0 to num_states - 1 do
	      for point = 0 to num_points - 1 do
		if isTrue (Array2.get a1 state point)
		then Model.Graph.iter_pred (fun state -> Array2.set slice state point valTrue) model.kripke state
	      done
	    done
	 | Eu (f1,f2) ->
	    Array2.fill slice valFalse;
	    let a1 = cache f1 in
	    let a2 = cache f2 in
	    let accum = ref [] in
	    for state = 0 to num_states - 1 do
	      for point = 0 to num_points - 1 do
 		if isTrue (Array2.get a2 state point) then
		  begin
		    Array2.set slice state point valTrue;
		    Model.Graph.iter_pred (fun state -> if isTrue (Array2.get a1 state point) &&
							     isFalse (Array2.get a2 state point) &&
							       isFalse (Array2.get slice state point)
							then (Array2.set slice state point valUtil; accum := (state,point)::(!accum)))
					  model.kripke state
		  end;		
	      done
	    done;	    
	    let rec fn () =
	      match !accum with
		[] -> ()
	      | (state,point)::xs ->
		 Array2.set slice state point valTrue;
		 Model.Graph.iter_pred (fun state -> if isTrue (Array2.get a1 state point) &&
							  isFalse (Array2.get a2 state point) &&
							    isFalse (Array2.get slice state point)
							then (Array2.set slice state point valUtil; accum := (state,point)::(!accum)))
				       model.kripke state;
	    in fn()		    
	);
	slice in
    fun f ->
    let slice = cache f in
    fun state point -> isTrue(Array2.get slice state point)
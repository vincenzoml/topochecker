open Logic
open Model
open Bigarray
open Util
       
let precompute model =
  let num_states = Graph.nb_vertex model.kripke in
  let num_points = Graph.nb_vertex model.space in
  let count = Array1.create int c_layout num_states in 
  let rec cache f =
    try
      H.find model.eval f 
    with Not_found ->
      let slice = Array2.create float64 c_layout num_states num_points in
      H.add model.eval f slice;
      let iter fn = for state = 0 to num_states - 1 do
		      for point = 0 to num_points - 1 do
			Array2.set slice state point (fn state point)
		      done
		      done in
      (match f with 
	 T -> Array2.fill slice valTrue
       | Prop p -> (* TODO check whether p really is in eval? *) ()
       | QProp (p,op,n) ->
	  let a1 = cache (Prop p) in
	  for state = 0 to num_states - 1 do
	    for point = 0 to num_points - 1 do
	      Array2.set slice state point (Util.ofBool (Syntax.opsem op (Array2.get a1 state point) n))
	    done
	  done
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
	  Array2.fill slice valFalse;
	  let a1 = cache f1 in
          let a2 = cache f2 in
          let accum = Stack.create () in
	  for state = 0 to num_states - 1 do
            for point = 0 to num_points - 1 do
	      Array2.set slice state point (Array2.get a1 state point);
	      if isTrue (Array2.get a1 state point) || isTrue (Array2.get a2 state point) then
		Model.Graph.iter_succ
		  (fun point -> if (isFalse (Array2.get a1 state point)) &&
				     (isFalse(Array2.get a2 state point)) &&
				       (isFalse(Array2.get slice state point))
				then (Array2.set slice state point valUtil; Stack.push point accum))
		  model.space point
	    done;
	    while not (Stack.is_empty accum) do
	      let point = Stack.pop accum in
	      Array2.set slice state point valFalse;
	      Model.Graph.iter_pred (fun point ->
				     if isTrue (Array2.get slice state point) then
				       begin
					 Array2.set slice state point valFalse;
					 if isFalse (Array2.get a2 state point)
					 then Stack.push point accum
				       end
				    ) model.space point
	    done;
	  done;
       | Af f1 ->
	  let a1 = cache f1 in
	  for point = 0 to num_points - 1 do
	    let accum = Stack.create () in		  	    
	    for state = 0 to num_states - 1 do
	      if Util.isTrue (Array2.get a1 state point)
	      then (Array1.set count state 0; Array2.set slice state point valTrue; Stack.push state accum)
	      else Array1.set count state (Model.Graph.out_degree model.kripke state)
	    done;
	    while not (Stack.is_empty accum) do
	      let state = Stack.pop accum in
	      Model.Graph.iter_pred
		(fun state ->
		 let c = Array1.get count state in
		 if c > 0 then Array1.set count state (c-1);		 
		 if c = 1 then (Stack.push state accum;
				Array2.set slice state point valTrue))
		model.kripke state
	    done	      
	  done;	  
       | Ex f1 ->
	  Array2.fill slice valFalse;
	  let a1 = cache f1 in
	  for state = 0 to num_states - 1 do
	    for point = 0 to num_points - 1 do
	      if isTrue (Array2.get a1 state point)
	      then Model.Graph.iter_pred
		     (fun state -> Array2.set slice state point valTrue)
		     model.kripke state
	    done
	  done
       | Eu (f1,f2) ->
	  Array2.fill slice valFalse;
	  let a1 = cache f1 in
	  let a2 = cache f2 in
	  let accum = Stack.create () in
	  for state = 0 to num_states - 1 do
	    for point = 0 to num_points - 1 do
 	      if isTrue (Array2.get a2 state point) then
		begin
		  Array2.set slice state point valTrue;
		  Model.Graph.iter_pred
		    (fun state -> if isTrue (Array2.get a1 state point) &&
				       isFalse (Array2.get a2 state point) &&
					 isFalse (Array2.get slice state point)
				  then (Array2.set slice state point valUtil;
					Stack.push (state,point) accum))
		    model.kripke state
		end;		
	    done
	  done;	    
	  while not (Stack.is_empty accum) do
	    let (state,point) = Stack.pop accum in
	    Array2.set slice state point valTrue;
	    Model.Graph.iter_pred
	      (fun state -> if isTrue (Array2.get a1 state point) &&
				 isFalse (Array2.get a2 state point) &&
				   isFalse (Array2.get slice state point)
			    then (Array2.set slice state point valUtil;
				  Stack.push (state,point) accum))
	      model.kripke state;
	  done
      );
      slice in
  fun f ->
  let slice = cache f in
  fun state point -> isTrue(Array2.get slice state point)
			   

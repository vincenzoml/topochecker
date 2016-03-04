open Logic
open Model
open Bigarray
open Util
       
let precompute model =
  let num_states = Graph.nb_vertex model.kripke in
  let num_points =  model.space.num_nodes in
  let count = Array1.create int c_layout num_states in
  let rec cache f =
    try
      H.find model.eval f 
    with Not_found ->
      match f with
	Coll cf ->
      (* CH.add model.collective_eval cf (check_collective_formula cache cf model.space ) *)
	(fun state point -> ofBool (point mod 2 = 0))
      | f ->
	 let slice = Array2.create float64 c_layout num_states num_points in
	 H.add model.eval f (Array2.get slice);
	 let iter fn = for state = 0 to num_states - 1 do
			 for point = 0 to num_points - 1 do
			   Array2.set slice state point (fn state point)
			 done
		       done in
	 (match f with 
	    T -> Array2.fill slice valTrue			     
	  | Prop p -> (* TODO check whether p really is in eval? *) ()
	  | Coll cf -> () (* TODO this case is not possible, but the compiler would complain, fix *)
	  | VProp (p,op,n) ->
	     let a1 = cache (Prop p) in
	     for state = 0 to num_states - 1 do
	       for point = 0 to num_points - 1 do
		 Array2.set slice state point (Util.ofBool (Syntax.opsem op (a1 state point) n))
	       done
	     done
	  | Not f1 -> let a1 = cache f1 in
		      iter (fun state point -> valNot (a1 state point))
	  | And (f1,f2) -> let a1 = cache f1 in
			   let a2 = cache f2 in
			   iter (fun state point -> valAnd (a1 state point) (a2 state point))
	  | Near f1 ->
	     Array2.fill slice valFalse;
	     let a1 = cache f1 in
	     for state = 0 to num_states - 1 do
	       for point = 0 to num_points - 1 do
		 if isTrue (a1 state point) then
		   begin
		     Array2.set slice state point valTrue;
		     model.space.iter_post point
					   (fun point' ->
					    Array2.set slice state point' valTrue)
		   end
	       done
	  done
	  | Surrounded (f1,f2) ->
	     Array2.fill slice valFalse;
	     let a1 = cache f1 in
             let a2 = cache f2 in
             let accum = Stack.create () in
	     for state = 0 to num_states - 1 do
               for point = 0 to num_points - 1 do
		 Array2.set slice state point (a1 state point);
		 if isTrue (a1 state point) || isTrue (a2 state point) then
		   model.space.iter_post point
					 (fun point -> if (isFalse (a1 state point)) &&
							    (isFalse(a2 state point)) &&
							      (isFalse(Array2.get slice state point))
						       then (Array2.set slice state point valUtil;
							     Stack.push point accum))
	       done;
	       while not (Stack.is_empty accum) do
		 let point = Stack.pop accum in
		 Array2.set slice state point valFalse;
		 model.space.iter_pre point (fun point ->
					     if isTrue (Array2.get slice state point) then
					       begin
						 Array2.set slice state point valFalse;
						 if isFalse (a2 state point)
						 then Stack.push point accum
					       end)
	       done;
	     done;
	  | Af f1 ->
	     let a1 = cache f1 in
	     for point = 0 to num_points - 1 do
	       let accum = Stack.create () in		  	    
	       for state = 0 to num_states - 1 do
		 if Util.isTrue (a1 state point)
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
	      if isTrue (a1 state point)
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
 		 if isTrue (a2 state point) then
		   begin
		     Array2.set slice state point valTrue;
		     Model.Graph.iter_pred
		       (fun state -> if isTrue (a1 state point) &&
					  isFalse (a2 state point) &&
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
		 (fun state -> if isTrue (a1 state point) &&
				    isFalse (a2 state point) &&
				      isFalse (Array2.get slice state point)
			       then (Array2.set slice state point valUtil;
				     Stack.push (state,point) accum))
		 model.kripke state;
	     done
	 );
	 (Array2.get slice) in
  fun f ->
  match f with
    Prop "deadlock" -> 
    fun state point ->
    (match model.deadlocks with
       None -> false
     | Some f -> f state)
  | _ ->
     let slice = cache f in
     fun state point -> isTrue (slice state point)
  			       
let rec qchecker points nb checker qf =
  match qf with
  | QFloat f -> f
  | QOp (qop,f1,f2) -> ofBool (qop (qchecker points nb checker f1) (qchecker points nb checker f2))
  | QCount f -> float_of_int (let res = ref 0 in
			      let c = checker f 0 in
			      if [] = points then
				for i = 0 to nb-1 do 
				  if c i
				  then res := !res + 1
				done
			      else
				List.iter
				  (fun i ->
				   if c i
				    then res := !res + 1)
				  points;
			      !res)

			     
			     

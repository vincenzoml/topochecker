open Logic
open Model
open Bigarray
open Util

module IntOrdDst : sig type t=(float*int) val compare: (float*int) -> (float*int) -> int end = struct
  type t = (float*int)
  let compare = Pervasives.compare
end
module DDTSet = Set.Make(IntOrdDst)
       
let precompute model =
  let num_states = Graph.nb_vertex model.kripke in
  let num_points =  model.space.num_nodes in
  let count = Array1.create int c_layout num_states in (* auxiliary *)
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
	  | Statcmp (p,f,rad,op,thr,min,max,nbins) ->
	     (match model.iter_ball with
		None -> Util.fail "model does not have distances but SCMP operator used"
	      | Some ib ->
		 let step = (max -. min) /. (float_of_int nbins) in
		 let v1 = Array.make nbins 0 in
		 let v2 = Array.make nbins 0 in
		 let a1 = cache (Prop p) in
		 let a2 = cache f in
		 let bin bins value =		   
		   if (value < min) || (value >= max) then ()
		   else let i = (int_of_float ((value -. min) /. step)) in
			bins.(i) <- bins.(i) + 1;
		 in
		 for state = 0 to num_states - 1 do
		   Util.reset v2 0;
		   for point = 0 to num_points - 1 do
		     if Util.isTrue (a2 state point) then bin v2 (a1 state point) else ()
		   done;
		   for point = 0 to num_points - 1 do
		     Util.reset v1 0;
		     ib point rad (fun point -> bin v1 (a1 state point));
		     let res = Util.statcmp v1 v2 in
		     Array2.set slice state point (Util.ofBool (Syntax.opsem op res thr))
		   done
		 done)
	  | Eucl (f,op,thr) ->
	     (match model.euclidean_distance with
	     | None -> Util.fail "model does not have distances but EUCL operator used"
	     | Some ib ->
		let a1 = cache f in
		for state = 0 to num_states - 1 do
		  let edgeS = Util.edge (a1 state) model.space in
		  for point = 0 to num_points - 1 do
		    let dst = ref infinity in
		    if PointsSet.mem point edgeS then
		      dst := 0.0
		    else
		      Util.PointsSet.iter
		  	(fun e ->
		  	  let s = if isTrue (a1 state point) then -1.0 else 1.0 in
		  	  let d = ib point e in
		  	  if (d < abs_float(!dst)) then dst := d*.s) edgeS;
		  Array2.set slice state point (Util.ofBool (Syntax.opsem op !dst thr))
		  done
		done
	     )
	  | ModDijkstraDT (f,op,thr) ->
	     let a1 = cache f in
	     Array2.fill slice infinity;
	     for state = 0 to num_states - 1 do
	       let edgeS = Util.edge (a1 state) model.space in
	       let lSet = ref DDTSet.empty in
	       Util.PointsSet.iter (fun e -> lSet:=DDTSet.add (0.0,e) !lSet;Array2.set slice state e 0.0) edgeS;
	       while (not (DDTSet.is_empty !lSet)) do
		 let e = DDTSet.min_elt !lSet in
		 lSet:=DDTSet.remove e !lSet;
		 match e with
		 | (dst,point) ->
		    model.space.iter_post point (*pre or post?*)
		      (fun p w ->
			let dn=Array2.get slice state p in
			if dn>(dst+.w) then
			  begin
			    Array2.set slice state p (dst+.w);
			    lSet:=DDTSet.add (dst+.w,p) !lSet;
			  end)
	       done;
	       for point = 0 to num_points - 1 do
		 let s=
		   if isTrue (a1 state point) then
		     -1.0
		   else
		     1.0 in
		 let d = s*.(Array2.get slice state point) in
		 Array2.set slice state point (Util.ofBool (Syntax.opsem op d thr))
	       done
	     done
	  | Near f1 ->
	     Array2.fill slice valFalse;
	     let a1 = cache f1 in
	     for state = 0 to num_states - 1 do
	       for point = 0 to num_points - 1 do
		 if isTrue (a1 state point) then
		   begin
		     Array2.set slice state point valTrue;
		     model.space.iter_post point
					   (fun point' w ->
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
					 (fun point w -> if (isFalse (a1 state point)) &&
							    (isFalse(a2 state point)) &&
							      (isFalse(Array2.get slice state point))
						       then (Array2.set slice state point valUtil;
							     Stack.push point accum))
	       done;
	       while not (Stack.is_empty accum) do
		 let point = Stack.pop accum in
		 Array2.set slice state point valFalse;
		 model.space.iter_pre point (fun point w ->
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

			     
			     

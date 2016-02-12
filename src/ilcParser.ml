open Graph.Dot_ast
open Bigarray
        
let load_ilc_model dir k s e =
  if k <> "" then Util.fail "Support for temporal reasoning in ILC models is not yet implemented.";
  if e <> "" then Util.fail "Atomic propositions in ILC models are embedded in the space description and should not be specified in external files.";  
  let filename = Util.mkfname dir s in
  let f = open_in filename in
  let g = Model.Graph.create () in
  let h = Hashtbl.create 10 in  
  (try
     while true do
       let l = String.trim(input_line f) in
       if String.get l 0 <> '#'
       then let l =
	      List.map (fun s ->
		match List.rev (BatString.nsplit s " ") with
		  num::kind::rest ->
		    (int_of_string num)
		      kind
		      (BatString.concat " " (List.rev (kind::rest))))
		(BatString.nsplit l "\t")
	    in
	    let c = ref 0 in
	    List.iter (fun l ->
	      match l with
		[] -> ()
	      | (value,kind,name)::xs ->
		 if not (Hashtbl.mem h name) then
		   begin
		     Hashtbl.add h name c;
		     c := !c + 1;
		   end
     done
   with End_of_file -> ());
  Util.fail "stub"
      

      (*{ Model.kripke = kripke;
	Model.space = space;
	Model.deadlocks = None;
    Model.write_output = write_dot_model;
    kripkeid = k_id_of_int;
    idkripke = k_int_of_id;
    spaceid = s_id_of_int;
    idspace = s_int_of_id;
    Model.local_state = ();
    Model.eval = propTbl }
      *)

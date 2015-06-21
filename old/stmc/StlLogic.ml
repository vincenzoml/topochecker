(** Segnatura del modello **)
open Util

module type MODEL = sig
    (* space *)    
    type space
    type space_point
    type space_pointset
    
    val string_of_space_point : space_point -> string
    val string_of_space_pointset : space_pointset -> string
      
    val space_mem : space_point -> space_pointset -> bool
    val space_singleton : space_point -> space_pointset
    val space_choose : space_pointset -> space_point
    val space_add : space_point -> space_pointset -> space_pointset
    val space_diff : space_pointset -> space_pointset -> space_pointset
    val space_subset : space_pointset -> space_pointset -> bool
    val space_inter : space_pointset -> space_pointset -> space_pointset
    val space_union : space_pointset -> space_pointset -> space_pointset
    val space_complement : space_pointset -> space -> space_pointset
    val space_filter : (space_point -> bool) -> space_pointset -> space_pointset
    val space_iter : (space_point -> unit) -> space_pointset -> unit
    val space_fold : (space_point -> 'a -> 'a) -> space_pointset -> 'a -> 'a

    val space_domain : space -> space_pointset
    val space_empty : space_pointset

    val space_pred : space_point -> space -> space_pointset
    val space_next : space_point -> space -> space_pointset
    val space_closure : space_pointset -> space -> space_pointset

    (* time *)
    type time
    type time_point
    type time_pointset

    val string_of_time_point : time_point -> string
    val string_of_time_pointset : time_pointset -> string

    val time_mem : time_point -> time_pointset -> bool
    val time_singleton : time_point -> time_pointset
    val time_add : time_point -> time_pointset -> time_pointset
    val time_subset : time_pointset -> time_pointset -> bool
    val time_inter : time_pointset -> time_pointset -> time_pointset
    val time_union : time_pointset -> time_pointset -> time_pointset
    val time_diff : time_pointset -> time_pointset -> time_pointset
    val time_choose : time_pointset -> time_point
    val time_complement : time_pointset -> time -> time_pointset
    val time_remove : time_point -> time_pointset -> time_pointset
    val time_filter : (time_point -> bool) -> time_pointset -> time_pointset
    val time_iter : (time_point -> unit) -> time_pointset -> unit
    val time_fold : (time_point -> 'a -> 'a) -> time_pointset -> 'a -> 'a

    val time_pred : time_point -> time -> time_pointset
    val time_next : time_point -> time -> time_pointset

    val time_domain : time -> time_pointset
    val time_empty : time_pointset

    (* space-time *)
    type st
    type st_point
    type st_pointset

    val string_of_st_point : st_point -> string
    val string_of_st_pointset : st_pointset -> string

    val st_cardinal : st_pointset -> int
    val st_mem : st_point -> st_pointset -> bool
    val st_choose : st_pointset -> st_point
    val	st_add : st_point -> st_pointset -> st_pointset
    val st_remove : st_point -> st_pointset -> st_pointset
    val st_subset : st_pointset -> st_pointset -> bool
    val st_diff : st_pointset -> st_pointset -> st_pointset
    val st_inter : st_pointset -> st_pointset -> st_pointset
    val st_union : st_pointset -> st_pointset -> st_pointset
    val st_complement : st_pointset -> st -> st_pointset
    val st_filter : (st_point -> bool) -> st_pointset -> st_pointset
    val st_iter : (st_point -> unit) -> st_pointset -> unit
    val st_fold : (st_point -> 'a -> 'a) -> st_pointset -> 'a -> 'a
    val st_compare : st_point -> st_point -> int

    val st_make : space -> time -> st
    val st_make_point : space_point -> time_point -> st_point
    val st_space_section : st_pointset -> time_point -> space_pointset
    val st_time_section : st_pointset -> space_point -> time_pointset
    val st_cartesian_product : space_pointset -> time_pointset -> st_pointset

    val st_space_closure : st_pointset -> st -> st_pointset

    val st_time_pred : st_point -> st -> st_pointset
    val st_time_next : st_point -> st -> st_pointset

    val st_domain : st -> st_pointset
    val st_empty : st_pointset

    val st_space : st -> space
    val st_time : st -> time

    val st_to_space : st_point -> space_point
    val st_to_time : st_point -> time_point
end


(* segnatura delle proposizioni *)
module type PROP = sig
  type t
  type t_sem
  type env = t -> t_sem

  val string_of : t -> string
  val string_of_sem : t_sem -> string
  val compare : t -> t -> int
  
  val empty_env : env
  val bind : t -> t_sem -> env -> env
end


(** Modulo della logica **)
module Logic (Model : MODEL) (Prop : PROP with type t_sem = Model.st_pointset) =
  struct

  (** error handling **)
  let meta_variable_error ide = failwith (Printf.sprintf "meta variable in conversion from fsyntax to formula: %s" ide)
  let parameters_error () = failwith "wrong number of parameters passed to macro, or macro parameter called with arguments"

  (** formule **)
  type 'a formula =
    T
  | Prop of 'a
  | Not of 'a formula
  | And of ('a formula * 'a formula)
  | Or of ('a formula * 'a formula)
  | N of 'a formula
  | S of ('a formula * 'a formula)
  | Ex of 'a formula
  | Af of 'a formula
  | Eu of ('a formula * 'a formula)

  (* Il tipo delle variabili per formule e delle metavariabili *)
  type ide = string;;

  (** fsyntax **)
  type 'a fsyntax =
    TRUE
  | FALSE
  | PROP of Prop.t
  | NOT of 'a fsyntax
  | AND of ('a fsyntax * 'a fsyntax)
  | OR of ('a fsyntax * 'a fsyntax)
  | NEAR of 'a fsyntax
  | SURR of ('a fsyntax * 'a fsyntax)
  | AX of 'a fsyntax
  | EX of 'a fsyntax
  | AF of 'a fsyntax
  | EF of 'a fsyntax
  | AG of 'a fsyntax
  | EG of 'a fsyntax
  | AU of ('a fsyntax * 'a fsyntax)
  | EU of ('a fsyntax * 'a fsyntax)
  | CALL of (ide * ('a fsyntax) list)

  (* conversione a stringa *)
  let rec string_of_fsyntax f =
    match f with
      TRUE -> "T"
    | FALSE -> "F"
    | PROP a -> Printf.sprintf "<%s>" (Prop.string_of a)
    | NOT f1 -> Printf.sprintf "!%s" (string_of_fsyntax f1)
    | AND (f1,f2) -> Printf.sprintf "( %s & %s )" (string_of_fsyntax f1 ) (string_of_fsyntax f2 )
    | OR (f1,f2) -> Printf.sprintf "( %s | %s )" (string_of_fsyntax f1 ) (string_of_fsyntax f2 )
    | NEAR f1 -> Printf.sprintf "N %s" (string_of_fsyntax f1 )
    | SURR (f1,f2) -> Printf.sprintf "S [ %s , %s ]" (string_of_fsyntax f1 ) (string_of_fsyntax f2 )
    | AX f1 -> Printf.sprintf "AX %s" (string_of_fsyntax f1 )
    | EX f1 -> Printf.sprintf "EX %s" (string_of_fsyntax f1 )
    | AF f1 -> Printf.sprintf "AF %s" (string_of_fsyntax f1 )
    | EF f1 -> Printf.sprintf "EF %s" (string_of_fsyntax f1 )
    | AG f1 -> Printf.sprintf "AG %s" (string_of_fsyntax f1 )
    | EG f1 -> Printf.sprintf "EG %s" (string_of_fsyntax f1 )
    | AU (f1,f2) -> Printf.sprintf "AU [ %s , %s ]" (string_of_fsyntax f1 ) (string_of_fsyntax f2 )
    | EU (f1,f2) -> Printf.sprintf "EU [ %s , %s ]" (string_of_fsyntax f1 ) (string_of_fsyntax f2 )
    | CALL (id,fl) -> Printf.sprintf "$%s%s" id (string_of_arglist fl )

  and string_of_arglist args =
    match args with 
      [] -> ""
    | _ -> Printf.sprintf "[%s]" (string_of_arglist_inner args)
      
  and string_of_arglist_inner args =
    match args with 
      [] -> ""
    | [x] -> string_of_fsyntax x
    | x::xs -> Printf.sprintf "%s,%s" (string_of_fsyntax x ) (string_of_arglist_inner xs )    

  (** ambienti **)

  (* Env identifica gli ambienti *)
  module Env = Map.Make(String);;
  module MvarSet = Set.Make(String);;

  (* tipo delle formule con parametri formali *)
  type 'a parametric_fsyntax = 'a fsyntax * ide list;;

  (** un ambiente è una mappa Map(key mide,entry parametric_fsyntax ) **)
  (* ambiente vuoto generico *)
  let empty_env = Env.empty;;


  (* funzioni di stampa *)
  let print_env = fun env ->
    let print_head = fun x lv ->
      if lv = [] then print_string ( x ^ " --> ")
      else (print_string ( x ^ " "); print_string ((String.concat " " lv)^" --> ")) in
    let print_tail = fun y -> print_string (string_of_fsyntax y); in
    let nice_print = fun x y -> let (fr,varlist) = y in
				print_head x varlist;
				print_tail fr;
				print_newline() in
    Env.iter nice_print env


  
  (** modifica dell'ambiente **)
  (* Aggiunge o modifica la coppia (id,pfs) all'ambiente dove id è un nome di variabile formula, pfs è un parametric_fsyntax *)
  let bind_mvar =
    fun id fs pl env -> Env.add id (fs,pl) env;;


  (* funzione di sostituzione per meta variabili *)
  let rec sub_mvar =
    fun env big_fs p small_fs -> 
    match big_fs with
      TRUE -> TRUE
    | FALSE -> FALSE
    | PROP x -> PROP x
    | NOT f1 -> NOT ( sub_mvar env f1 p small_fs )
    | AND (f1,f2) -> AND ( sub_mvar env f1 p small_fs , sub_mvar env f2 p small_fs )
    | OR (f1,f2) -> OR (sub_mvar env f1 p small_fs , sub_mvar env f2 p small_fs)
    | NEAR f1 -> NEAR ( sub_mvar env f1 p small_fs )
    | SURR (f1,f2) -> SURR ( sub_mvar env f1 p small_fs , sub_mvar env f2 p small_fs )
    | AX f1 -> AX (sub_mvar env f1 p small_fs)
    | EX f1 -> EX (sub_mvar env f1 p small_fs)
    | AF f1 -> AF (sub_mvar env f1 p small_fs)
    | EF f1 -> EF (sub_mvar env f1 p small_fs)
    | AG f1 -> AG (sub_mvar env f1 p small_fs)
    | EG f1 -> EG (sub_mvar env f1 p small_fs)
    | AU (f1,f2) -> AU (sub_mvar env f1 p small_fs , sub_mvar env f2 p small_fs)
    | EU (f1,f2) -> EU (sub_mvar env f1 p small_fs , sub_mvar env f2 p small_fs)
    (* identificatori per formule *)
    | CALL ( id1 , fsl ) -> 
       if (p=id1) then 
	 match fsl with
	   [] -> small_fs
	 | _ -> parameters_error ()
       else CALL(id1, List.map (fun x -> (sub_mvar env x p small_fs)) fsl )
  
  and sub_mvar_list =
  	fun env form pl small_fsl -> match (pl,small_fsl) with
	  ( [] , [] ) -> form
	| ( p::ps , f::fs ) -> sub_mvar_list env (sub_mvar env form p f) ps fs
	| _ -> parameters_error ()

  (* conversione da fsyntax a formula *)
  let rec fsyntax_to_formula env f = 
    match f with
      TRUE -> T
    | FALSE -> Not T
    | PROP a -> Prop a
    | NOT f1 -> Not (fsyntax_to_formula env f1)
    | AND (f1,f2) -> And ( (fsyntax_to_formula env f1) , (fsyntax_to_formula env f2) )
    | OR (f1,f2) ->  Or ( fsyntax_to_formula env f1 , fsyntax_to_formula env f2 )
    | NEAR f1 -> N (fsyntax_to_formula env f1)
    | SURR (f1,f2) -> S (fsyntax_to_formula env f1,fsyntax_to_formula env f2)
    | AX f1 -> Not (Ex (Not (fsyntax_to_formula env f1) ) )
    | EX f1 -> Ex (fsyntax_to_formula env f1)
    | AF f1 -> Af (fsyntax_to_formula env f1)
    | EF f1 -> Eu (T , (fsyntax_to_formula env f1) )
    | AG f1 -> Not (Eu (T ,Not (fsyntax_to_formula env f1) ) )
    | EG f1 -> Not ( Af ( Not (fsyntax_to_formula env f1) ) )
    | AU (f1,f2) -> let (phi,psi) = (fsyntax_to_formula env f1,fsyntax_to_formula env f2) in
		    And( Not (Eu ( Not psi , And(Not phi,Not psi ) )) , Af psi )
    | EU (f1,f2) -> Eu ( (fsyntax_to_formula env f1) , (fsyntax_to_formula env f2) )
(*    | CALL (id,fl) -> let (f1,pl) = Env.find id env in
		      if List.length fl != List.length pl 
		      then parameter_error id
		      else 
			(let env1 = (List.fold_left (fun ev (ide,exp) -> Prop.bind ide exp ev) 
						    env (Util.zip fl pl)) in
			 fsyntax_to_formula env1 f1)  *)
    | CALL (id,fl) -> let (f1,pl) = Env.find id env in
		      fsyntax_to_formula env (sub_mvar_list env f1 pl fl)	      

  (** funzione di debug **)
  let rec string_of_formula = fun fr ->
    match fr with
    | T -> "T"
    | Prop(x) -> Printf.sprintf "Prop(%s)" (Prop.string_of x)
    | Not(f) -> Printf.sprintf "Not(%s)" (string_of_formula f)
    | And(f1,f2) -> Printf.sprintf "(%s) And (%s)" (string_of_formula f1) (string_of_formula f2)
    | Or(f1,f2) -> Printf.sprintf "(%s) Or (%s)" (string_of_formula f1) (string_of_formula f2)
    | N(f) -> Printf.sprintf "Near(%s)" (string_of_formula f)
    | S(f1,f2) -> Printf.sprintf "(%s) Surr (%s)" (string_of_formula f1) (string_of_formula f2)
    | Ex(f) -> Printf.sprintf "Ex(%s)" (string_of_formula f)
    | Af(f) -> Printf.sprintf "Af(%s)" (string_of_formula f)
    | Eu(f1,f2) -> Printf.sprintf "(%s) Eu (%s)" (string_of_formula f1) (string_of_formula f2)

  let debug_string = fun fr time ->
    Printf.sprintf "formula: %s ... Tempo: %f\n" (string_of_formula fr) time

  (* mappa punti -> punti *)
  module StPoint = struct
    type t = Model.st_point
    let compare = Model.st_compare
  end
  module PtP = Map.Make(StPoint)

  (* mappa proposizione -> valutazione *)
  module PrMap = Map.Make(Prop)

  (** funzioni semantiche **)
  let rec sem = fun form model pr_sem -> 
    sem_aux form model pr_sem (ref PrMap.empty)

  and sem_aux = fun form model pr_sem pr_map ->    
    let sem_out =
      match form with
	T -> Model.st_domain model
      | Prop a -> if PrMap.mem a (!pr_map)
		  then PrMap.find a (!pr_map)
		  else let _ = pr_map := PrMap.add a (pr_sem a) (!pr_map) in
		       PrMap.find a (!pr_map)
      | Not f1 -> Model.st_complement (sem_aux f1 model pr_sem pr_map) model
      | And (f1,f2) -> Model.st_inter (sem_aux f1 model pr_sem pr_map) (sem_aux f2 model pr_sem pr_map)
      | Or (f1,f2) -> Model.st_union (sem_aux f1 model pr_sem pr_map) (sem_aux f2 model pr_sem pr_map)
      | N f1 -> let phiset = sem_aux f1 model pr_sem pr_map in
		sem_n phiset model
      | S (f1,f2) -> let psiset = sem_aux f2 model pr_sem pr_map in
		     let phiset = sem_aux f1 model pr_sem pr_map in
		     sem_s phiset psiset model
      | Ex f1 -> let phiset = sem_aux f1 model pr_sem pr_map in
		 sem_ex phiset model
      | Af f1 -> let acc = ref(sem_aux f1 model pr_sem pr_map) in
    		 let bad = ref(Model.st_complement (!acc) model) in
    		 (* let control = ref(if (!todo) = Model.st_empty then false else true) in *)
    		 let _ = sem_af acc bad model in
    		 !acc
      | Eu (f1,f2) -> let acc = sem_aux f2 model pr_sem pr_map in
		      let phiset = sem_aux f1 model pr_sem pr_map in
		      sem_eu acc phiset model
    in
    sem_out

  (* semantica n *)
  and sem_n = fun phiset model ->
    Model.st_space_closure phiset model

  (* semantica s *)
  and sem_s_aux = fun p q space ->
    let r = ref p in
    let pORq = Model.space_union p q in
    let t = ref (Model.space_diff (Model.space_closure pORq space) pORq) in
    while not (Model.space_empty = (!t)) do
      let x = Model.space_choose (!t) in
      let n = Model.space_diff (Model.space_inter (Model.space_pred x space) (!r)) q in
      r := Model.space_diff (!r) n;
      t := Model.space_diff (Model.space_union (!t) n) (Model.space_singleton x)
    done;
    (!r)

  and sem_s = fun phiset psiset model ->
    let (space,time) = (Model.st_space model,Model.st_time model) in
    let tdom = Model.time_domain time in
    let phi_sec = Model.st_space_section phiset in
    let psi_sec = Model.st_space_section psiset in
    let smart_fold = fun t stpset -> 
      let p = phi_sec t in
      let q = psi_sec t in
      let cl_section_t = sem_s_aux p q space in
      Model.space_fold (fun s stset -> Model.st_add (Model.st_make_point s t) stset) cl_section_t stpset
      in
    Model.time_fold smart_fold tdom Model.st_empty

  (* semantica ex *)
  and sem_ex = fun phiset model ->
    let smart_union = fun x sts -> Model.st_union (Model.st_time_pred x model) sts in
    Model.st_fold smart_union phiset Model.st_empty
    
  (* semantica af - old implementation *)
  (* and sem_af_aux = fun acc todo control stref x -> *)
  (*   let nset = Model.st_time_next x (!stref) in *)
  (*   if Model.st_subset nset (!acc) *)
  (*   then ( *)
  (*     acc := Model.st_add x (!acc); *)
  (*     todo := Model.st_remove x (!todo); *)
  (*     control := true *)
  (*   ) *)
  (*   else () *)

  (* and sem_af = fun acc todo control stref -> *)
  (*   if (!control) *)
  (*   then ( *)
  (*     control := false; *)
  (*     Model.st_iter (sem_af_aux acc todo control stref) (!todo); *)
  (*     sem_af acc todo control stref *)
  (*   ) *)
  (*   else () *)

  (* semantica af *)
  and sem_af_count = fun acc bad todo model ->
    let count = ref PtP.empty in
    let smart_add_count = fun st ->
      count := PtP.add st (Model.st_cardinal (Model.st_time_next st model)) (!count)
    in
    let _ = Model.st_iter smart_add_count (!bad) in
    count

  and sem_af_aux = fun acc todo count model x ->
    let _ = todo := Model.st_empty in
    let pred_elaboration = fun y ->
      try
	let l = PtP.find y (!count) in
	count := PtP.add y (l-1) (!count);
	if (l=1)
	then (
	  todo := Model.st_add y (!todo);
	  acc := Model.st_add y (!acc);
	  count := PtP.remove y (!count);
	)
      with
      | Not_found -> ()
    in
    Model.st_iter pred_elaboration (Model.st_time_pred x model)

  and sem_af = fun acc bad model ->
    let todo = ref Model.st_empty in
    let count = sem_af_count acc bad todo model in
    let _ = Model.st_iter (sem_af_aux acc todo count model) (!acc) in
    while ((!todo) <> Model.st_empty) do
      Model.st_iter (sem_af_aux acc todo count model) (!todo)
    done

  (* semantica eu *)
  and sem_eu = fun acc phiset model ->
    let new_acc = Model.st_union acc (Model.st_inter (sem_ex acc model) phiset) in
    if Model.st_diff new_acc acc = Model.st_empty
    then new_acc
    else sem_eu new_acc phiset model

end

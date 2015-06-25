open Logic

type ide = string
type propsyn = string
type model = MODEL of (string*string*string) (* Kripke, Space, Eval *)
	     
type fsyn =
    TRUE
  | FALSE
  | NOT of fsyn
  | AND of (fsyn * fsyn)
  | OR of (fsyn * fsyn)
(*  | SHARE of (fsyn * fsyn)
  | GROUP of fsyn *)
  | PROP of propsyn
  | NEAR of fsyn
  | NEARN of (int * fsyn)
  | INT of fsyn
  | SURROUNDED of (fsyn * fsyn)
  | CALL of ide * (fsyn list)
  | EX of fsyn	    
  | AX of fsyn
  | EG of fsyn
  | AG of fsyn
  | EF of fsyn
  | AF of fsyn
  | EU of fsyn * fsyn
  | AU of fsyn * fsyn
		    
type decl = LET of ide * ide list * fsyn
type dseq = decl list

type com = 
  | CHECK of fsyn
(*  | CCHECK of fsyn
  | CCHECKSET of fsyn * ((int * int) list) *)
		      
type experiment = model * dseq * com

module MEnv = Map.Make(struct type t = ide let compare = compare end)		     
type dval = ide list * fsyn
type env = dval MEnv.t	       
let empty : env = MEnv.empty		
let bind : env -> ide -> dval -> env = fun env name value -> MEnv.add name value env
let apply : env -> ide -> dval = fun env name -> MEnv.find name env
		    
let zipenv : env -> ide list -> dval list -> env =
  fun env formals actuals ->
  List.fold_left
    (fun env (formal,actual) -> bind env formal actual)
    env
    (List.combine formals actuals)

exception TypeMismatch

let env_of_dseq ds = List.fold_left (fun env (LET (name,args,body)) -> bind env name (args,body)) empty ds  
					
let rec formula_of_fsyn env f =
  match f with
    PROP prop -> Prop prop
  | TRUE -> T
  | FALSE -> Not T
  | NOT f1 -> Not (formula_of_fsyn env f1)     
  | AND (f1,f2) -> And (formula_of_fsyn env f1,formula_of_fsyn env f2)
  | OR (f1,f2) -> Not (And (Not (formula_of_fsyn env f1),Not (formula_of_fsyn env f2)))
  | NEAR f1 -> Near (formula_of_fsyn env f1)
  | NEARN (n,f1) -> if n <= 0 then formula_of_fsyn env f1 else Near (formula_of_fsyn env (NEARN(n-1,f1)))
  | INT f1 -> Not (Near (Not (formula_of_fsyn env f1)))
  | SURROUNDED (f1,f2) -> Surrounded (formula_of_fsyn env f1,formula_of_fsyn env f2)
  | EX f1 -> Ex (formula_of_fsyn env f1)
  | AX f1 -> Not (Ex (Not (formula_of_fsyn env f1)))
  | EG f1 -> Not (Af (Not (formula_of_fsyn env f1)))
  | AG f1 -> Not (Eu (T,Not (formula_of_fsyn env f1)))
  | EF f1 -> Eu (T,formula_of_fsyn env f1)
  | AF f1 -> Af (formula_of_fsyn env f1)
  | EU (f1,f2) -> Eu (formula_of_fsyn env f1,formula_of_fsyn env f2)
  | AU (f1,f2) ->
     let (ff1,ff2) = (formula_of_fsyn env f1,formula_of_fsyn env f2) in
     And(Not (Eu (Not ff2,And(Not ff1,Not ff2))),Af ff2)
  | CALL (ide,actuals) ->
     let (formals,body) = apply env ide in
     formula_of_fsyn (zipenv env formals (List.map (fun x -> ([],x)) actuals)) body


(*  | SHARE _ -> raise TypeMismatch
  | GROUP _ -> raise TypeMismatch  

let rec cformula_of_fsyn env f =
  match f with
  | TRUE -> CT
  | FALSE -> CNot CT
  | NOT f1 -> CNot (cformula_of_fsyn env f1)     
  | AND (f1,f2) -> CAnd (cformula_of_fsyn env f1,cformula_of_fsyn env f2)
  | OR (f1,f2) -> CNot (CAnd (CNot (cformula_of_fsyn env f1),CNot (cformula_of_fsyn env f2)))
  | GROUP f1 -> CGroup (formula_of_fsyn env f1)			  
  | CALL (ide,actuals) ->
     let (formals,body) = apply env ide in
     cformula_of_fsyn (zipenv env formals (List.map (fun x -> ([],x)) actuals)) body
  | _ -> raise TypeMismatch
 *)

	       

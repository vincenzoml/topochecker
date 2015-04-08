open Color.Rgb

module type LANG = sig
  type syntax
  type semantics
  val string_of : syntax -> string
  val sem : syntax -> semantics
end

module Logic ( Prop : LANG) = struct
  type 'a formula = 
      T 
    | Prop of 'a
    | Not of 'a formula 
    | And of ('a formula * 'a formula) 
    | Closure of ('a formula) 
    | Until of ('a formula * 'a formula)
		 
   and 'a cformula =
       CT
     | CNot of ('a cformula)
     | CAnd of ('a cformula * 'a cformula)
     | Group of ('a formula)
     | Share of ('a formula * 'a cformula)
      
  module Env = Map.Make(String)

  type cfsyntax =
      CTRUE
    | CFALSE
    | CNOT of cfsyntax
    | CAND of (cfsyntax * cfsyntax)
    | COR of (cfsyntax * cfsyntax)
    | SHARE of (fsyntax * cfsyntax)
    | GROUP of fsyntax 
		       
  and fsyntax =
    TRUE 
  | FALSE
  | PROP of Prop.syntax
  | NOT of fsyntax
  | AND of (fsyntax * fsyntax)
  | OR of (fsyntax * fsyntax)
  | CLOS of fsyntax
  | CLOSN of (int * fsyntax)
  | INT of fsyntax
  | UNTIL of (fsyntax * fsyntax)
  | CALL of string * (fsyntax list) 
      
  let rec string_of_fsyntax f =
    match f with
      TRUE -> "T"
    | FALSE -> "F"
    | PROP prop -> Prop.string_of prop
    | NOT f -> Printf.sprintf "!(%s)" (string_of_fsyntax f)
    | AND (f1,f2) -> Printf.sprintf "(%s & %s)" (string_of_fsyntax f1) (string_of_fsyntax f2) 
    | OR (f1,f2) -> Printf.sprintf "(%s | %s)" (string_of_fsyntax f1) (string_of_fsyntax f2) 
    | CLOS f -> Printf.sprintf "(C %s)" (string_of_fsyntax f)
    | CLOSN (i,f) -> Printf.sprintf "(C^%d %s)" i (string_of_fsyntax f)
    | INT f -> Printf.sprintf "(I %s)" (string_of_fsyntax f)
    | UNTIL (f1,f2) -> Printf.sprintf "(%s U %s)" (string_of_fsyntax f1) (string_of_fsyntax f2) 
    | CALL (f,args) -> Printf.sprintf "%s%s" f (string_of_arglist args)
      
  and string_of_arglist args =
    match args with 
      [] -> ""
    | _ -> Printf.sprintf "(%s)" (string_of_arglist_inner args)
      
  and string_of_arglist_inner args =
    match args with 
      [] -> ""
    | [x] -> string_of_fsyntax x
    | x::xs -> Printf.sprintf "%s,%s" (string_of_fsyntax x) (string_of_arglist_inner xs)
      
  let rec fsyntax_sub f fsyntax =
    match fsyntax with
      CALL (ide,arglist) -> f ide
    | x -> x
      
  let rec zipenv env l1 l2 =
    match (l1,l2) with
      ([],[]) -> env
    | (x::xs,y::ys) -> Env.add x (fun [] -> y) (zipenv env xs ys)
    | _ -> raise (Failure "zipenv")
      
  type 'a myfun = 'a formula list -> 'a formula
    
  let rec fun_of_decl ide (env : 'a myfun Env.t) (formalargs : string list) (body : fsyntax) (actualargs : 'a formula list) =
    let newenv =
      try
	zipenv env formalargs actualargs
      with _ -> failwith (Printf.sprintf "wrong number of arguments in call to %s" ide) in
    formula_of_fsyntax newenv body
      
  and formula_of_fsyntax (env : 'a myfun Env.t) (fsyntax : fsyntax) =
    match fsyntax with
      PROP prop -> Prop (Prop.sem prop)
    | TRUE -> T
    | FALSE -> Not T
    | NOT t -> Not (formula_of_fsyntax env t)
    | AND (t1,t2) -> And (formula_of_fsyntax env t1,formula_of_fsyntax env t2)
    | OR (t1,t2) -> Not (And (Not (formula_of_fsyntax env t1), Not (formula_of_fsyntax env t2)))
    | CLOS t -> Closure (formula_of_fsyntax env t)
    | CLOSN (n,t) -> if n <= 0 then (formula_of_fsyntax env t) else Closure (formula_of_fsyntax env (CLOSN(n-1,t)))
    | INT t -> Not (Closure (Not (formula_of_fsyntax env t)))
    | UNTIL (t1,t2) -> Until (formula_of_fsyntax env t1,formula_of_fsyntax env t2)
    | CALL (ide,arglist) -> 
      try (Env.find ide env) (List.map (formula_of_fsyntax env) arglist)
      with _ -> failwith (Printf.sprintf "Unbound identifier: %s" ide)

  and cformula_of_cfsyntax (env : 'a myfun Env.t) (cfsyntax : cfsyntax) =
    match cfsyntax with
      CTRUE -> CT
    | CFALSE -> CNot (CT)
    | CNOT cf -> CNot (cformula_of_cfsyntax env cf)
    | CAND (cf1, cf2) -> CAnd (cformula_of_cfsyntax env cf1, cformula_of_cfsyntax env cf2)
    | COR (cf1, cf2) -> cformula_of_cfsyntax env (CNOT (CAND (CNOT cf1, CNOT cf2)))
    | GROUP f -> Group (formula_of_fsyntax env f)
    | SHARE (f,cf) -> Share (formula_of_fsyntax env f, cformula_of_cfsyntax env cf)
			 
  type syntax = 
      PAINT of (Prop.syntax * fsyntax)
    | ASK of cfsyntax
    | LET of string * (string list) * fsyntax
    | RESET
end

module Picture = struct      
  let almost n x y = (x > y - n) && (x < y + n)
    
  let tabulate list =
    let h = Hashtbl.create (List.length list) in
    List.iter (fun (k,v) -> Hashtbl.add h k v) list;
    Hashtbl.find h
      
  let unops =
    tabulate [("~",fun c y -> almost 7 c.r y.r && almost 7 c.g y.g && almost 7 c.b y.b)] 

  let binops =
    tabulate [("=",(=));
	      (">=",(>=));
	      ("<=",(<=));
	      ("<",(<));
	      (">",(>))]       
    
  type value =
    RED
  | GREEN
  | BLUE
  | NUM of int
      
  type color =
    COLOR of string
  | RGB of (int * int * int)

  type syntax =  
    COL of color
  | UNOP of (string * color)
  | BINOP of (value * string * value)
      
  let rec string_of_value value =
    match value with
      RED -> "R"
    | GREEN -> "G"
    | BLUE -> "B"
    | NUM i -> string_of_int i
      
  let rec string_of_color color =
    match color with
      COLOR s -> Printf.sprintf "\"s\""
    | RGB (r,g,b) -> Printf.sprintf "# %d %d %d" r g b

  let rec string_of syntax =
    match syntax with
      COL c -> string_of_color c
    | UNOP (op,c) -> Printf.sprintf "%s %s" op (string_of_color c)
    | BINOP (v1,op,v2) -> Printf.sprintf "%s %s %s" (string_of_value v1) op (string_of_value v2) 
      
  type semantics = Color.Rgb.t -> bool

  let rec feature_of_value value p =
    match value with
      RED -> p.Color.r
    | GREEN -> p.Color.g
    | BLUE -> p.Color.b
    | NUM i -> i
      
  let colorsem color =
    match color with
      COLOR s -> Color.color_parse s
    | RGB (r,g,b) -> { r = r ; g = g; b = b }

  let rec sem syn color =
    match syn with
    | COL c -> color = (colorsem c)
    | UNOP (op,c) -> unops op color (colorsem c)
    | BINOP (v1,op,v2) -> binops op (feature_of_value v1 color) (feature_of_value v2 color)
end

module PictureLogic = Logic(Picture)

module DMC (Point : Set.OrderedType) =
struct
  open PictureLogic

  module PSet = Set.Make(Point)
  type pointSet = PSet.t
  type point = PSet.elt

  type space = {
    points : pointSet;
    post : point -> pointSet;
    pre : point -> pointSet;
    clos : pointSet -> pointSet;
  }
    
  type 'a model = {
    space : space;
    eval : 'a -> pointSet;
  }
 
  let rec check model formula =
    match formula with
      T -> model.space.points
    | Prop p -> model.eval p
    | Not f -> PSet.diff model.space.points (check model f)
    | And (f1,f2) -> PSet.inter (check model f1) (check model f2)
    | Closure f -> model.space.clos (check model f)
    | Until (f1,f2) -> check_until model f1 f2 

  and check_until model f1 f2 =
    let (p,q) = (check model f1, check model f2) in
    let r = ref p in
    let pORq = PSet.union p q in
    let t = ref (PSet.diff (model.space.clos pORq) pORq) in
    while not (PSet.is_empty (!t)) do
      let x = PSet.choose (!t) in
      let n = PSet.diff (PSet.inter (model.space.pre x) (!r)) q in
      r := PSet.diff (!r) n;
      t := PSet.diff (PSet.union (!t) n) (PSet.singleton x)
    done;
    (!r)

  and ccheck model points cformula =
    match cformula with
      CT -> true
    | CAnd (cf1, cf2) -> (ccheck model points cf1) && (ccheck model points cf2)
    | CNot cf -> not (ccheck model points cf)
    | Share (f, cf) -> ccheck model (check model f) cf
    | Group f -> check_group model points f

  and check_group model points f =
    let rec my_visit visited to_visit points =
      if PSet.is_empty points then true
      else if PSet.is_empty to_visit then false
      else let x = PSet.choose to_visit in
	   let visited' = PSet.add x visited in
	   let to_visit' = PSet.diff
			     (PSet.union
				(PSet.union to_visit (model.space.pre x))
				(model.space.clos (PSet.singleton x)))
			     visited' in
	   let points' = PSet.remove x points in
	   my_visit visited' to_visit' points'
    in
    if PSet.is_empty points then true
    else let trap = check model f in
	 let ctrap = PSet.diff model.space.points trap in
	 if not (PSet.is_empty (PSet.inter points ctrap)) then false
	 else let trap_points = PSet.inter points trap in
	      if PSet.is_empty trap_points then false				
	      else let start = PSet.choose trap_points in
		   my_visit
		     ctrap
		     (PSet.singleton start)
		     points

  let from_coloured_graph nodes_props arcs = 
    let add_node tbl idx n =
      if Hashtbl.mem tbl idx 
      then Hashtbl.replace tbl idx (PSet.add n (Hashtbl.find tbl idx))
      else Hashtbl.add tbl idx (PSet.singleton n) in
    let get_nodes tbl idx = if Hashtbl.mem tbl idx then Hashtbl.find tbl idx else PSet.empty in
    let l = List.length nodes_props in
    let f = Hashtbl.create l in
    let b = Hashtbl.create l in
    let c = Hashtbl.create (3 * (List.length nodes_props) (* TODO: improve this estimate *) ) in
    List.iter (fun (s,t) -> 
      add_node f s t;
      add_node b t s;
    ) arcs;
    List.iter (fun (n,props) -> (List.iter (fun prop -> add_node c prop n) props)) nodes_props;
    let post = fun s -> get_nodes f s in
    { space = { 
      points = List.fold_left (fun res (n,_) -> PSet.add n res) PSet.empty nodes_props;
      post = post;
      pre = (fun t -> get_nodes b t);
      clos = (fun p -> PSet.fold (fun el res -> PSet.union (post el) res) p p)};
      eval = (fun n -> get_nodes c n)
    }     
end

module DigitalPlane = DMC (struct 
  type t = (int * int) 
  let compare = Pervasives.compare 
end) 

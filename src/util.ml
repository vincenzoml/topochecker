let isTrue x = x = 1.0
let isFalse x = x = 0.0
let valTrue = 1.0
let valFalse = 0.0
let valUtil = 2.0
let valAnd x y = if x = 1.0 && y = 1.0 then 1.0 else 0.0
let valNot x = if x = 0.0 then 1.0 else 0.0
let ofBool x = if x then valTrue else valFalse

module IntOrdT : sig type t=int val compare: int -> int -> int end = struct
  type t = int
  let compare = Pervasives.compare
end
module PointsSet = Set.Make(IntOrdT)

type simple_graph =
  { num_nodes : int;
    iter_pre : int -> (int -> float -> unit) -> unit;
    iter_post : int -> (int -> float -> unit) -> unit }
  
type connectivity = CityBlock | Chessboard | Euclidean | SubDim
					
let debug s =
  Printf.eprintf "debug: %s\n%!" s		 
		 
let fail s =
  Printf.eprintf "%s\n%!" s;
  exit 1
    
let mkfname dir file =
  if Filename.is_relative file then dir ^ Filename.dir_sep ^ file else file

let assert_samelen a b = assert (Array.length a = Array.length b)

let sqr x = x *. x

let int_of_coords coords dims =
  let (r,_,_) = Array.fold_left (fun (acc,fac,dim) coord -> (coord * fac + acc,dims.(dim)*fac,dim+1)) (0,1,0) coords in
  r

let coords_of_int i dims =
  let products = Array.make ((Array.length dims) - 1) dims.(0) in
  for i = 1 to Array.length products - 1 do
    products.(i) <- dims.(i) * products.(i-1)
  done;
  let res = Array.make (Array.length dims) 0 in
  let r = ref i in
  let q = ref 0 in
  let j = ref 0 in
  while !j <= Array.length products - 1 do
    let k = Array.length products - !j - 1 in
    let v = !r in
    q := v / products.(k);
    r := v mod products.(k);
    res.(k+1) <- !q;
    j := !j + 1;    
  done;
  res.(0) <- !r;
  res

let euclidean_distance v1 v2 pixdims =
  assert_samelen v1 v2;
  let res = ref 0.0 in
  for i = 0 to Array.length v1 - 1 do
    res := !res +. (sqr (float_of_int (v1.(i) - v2.(i)) *. pixdims.(i)));
  done;
  sqrt !res

let iter_neighbour connect dims pixdims i fn =
  let coords = coords_of_int i dims in
  let recdims =
    match connect with
    | SubDim -> Array.length coords - 2
    | a -> Array.length coords - 1
  in
  match connect with
  | CityBlock ->
     for i=0 to recdims do
       let (orig,x,y) = (coords.(i),coords.(i) - 1,coords.(i) + 1) in
       if x >= 0 then 
	 begin
	   coords.(i) <- x;
	   let id = int_of_coords coords dims in
	   fn id 1.0;
	 end;
       if y < dims.(i) then
	 begin
	   coords.(i) <- y;
	   let id = int_of_coords coords dims in
	   fn id 1.0;
	 end;
       coords.(i) <- orig
     done
  | a ->
     let cursor = Array.make (Array.length coords) 0 in
     let rec iter_hypercube_rec n c =
       let st = max 0 (coords.(n) - 1) in
       let en = min (coords.(n) + 1) (dims.(n) - 1) in
       if n = 0 && c > 0 then
	 begin 
	   for i = st to en do
	     cursor.(n) <- i;
	     let id = int_of_coords cursor dims in
	     fn id
	       (match connect with
	       | Chessboard -> 1.0;
	       | a -> euclidean_distance coords cursor pixdims;
	       )
	   done
	 end
       else if n>0 then
	 for i = st to en do
	   cursor.(n) <- i;
	   iter_hypercube_rec (n - 1) (c + abs(cursor.(n) - coords.(n)))
	 done
     in
     iter_hypercube_rec (Array.length dims - 1) 0
    
let in_range coord dims =
  assert_samelen coord dims;
  let res = ref true in
  let i = ref 0 in
  while (!i < Array.length coord) && !res do
    res := (!res && (0 <= coord.(!i)) && (coord.(!i) < dims.(!i)));
    i := !i+1
  done;
  !res

let vop1 op v =
  Array.init (Array.length v) (fun i -> op v.(i))	     
    
let vop2 op v1 v2 =
  assert_samelen v1 v2;
  Array.init (Array.length v1) (fun i -> op v1.(i) v2.(i))

(*similar code used in iter_neighbour. Join!*)    
let iter_hypercube dims coord radius fn =
  assert_samelen dims coord;
  let cursor = Array.make (Array.length coord) 0 in
  let ir = int_of_float radius in
  let rec iter_hypercube_rec n =
    let st = max 0 (coord.(n) - ir) in
    let en = min (coord.(n) + ir) (dims.(n) - 1) in
    if n = 0 then
      for i = st to en do
	cursor.(n) <- i;
	fn cursor
      done
    else
      for i = st to en do
	cursor.(n) <- i;
	iter_hypercube_rec (n - 1)
      done
  in
  iter_hypercube_rec (Array.length dims - 1)

let world2vox dist pixdims =
  let vs = Array.make (Array.length pixdims) 0 in
  for i = 0 to (Array.length pixdims - 1) do
      vs.(i) <- int_of_float (ceil (dist /. pixdims.(i)));
  done;
  vs
    
let iter_hypercube_w dims pixdims coord radius fn =
  assert_samelen dims coord;
  let cursor = Array.make (Array.length coord) 0 in
  let ir = world2vox radius pixdims in
  let rec iter_hypercube_rec n =
    let st = max 0 (coord.(n) - ir.(n)) in
    let en = min (coord.(n) + ir.(n)) (dims.(n) - 1) in
    if n = 0 then
      for i = st to en do
	cursor.(n) <- i;
	fn cursor
      done
    else
      for i = st to en do
	cursor.(n) <- i;
	iter_hypercube_rec (n - 1)
      done
  in
  iter_hypercube_rec (Array.length dims - 1)

let avg v =
  let len = Array.length v in
  let sum = ref 0 in
  for i = 0 to len - 1 do
    sum := !sum + v.(i) 
  done;
  (float_of_int !sum) /. (float_of_int len)

let statcmp v1 v2 =
  assert_samelen v1 v2;
  let nbins = Array.length v1 in
  assert (nbins > 0);
  let avg1 = avg v1 in
  let avg2 = avg v2 in
  let num = ref 0.0 in
  let den1 = ref 0.0 in
  let den2 = ref 0.0 in
  for i = 0 to nbins - 1 do
    let t1 = (float_of_int v1.(i)) -. avg1 in
    let t2 = (float_of_int v2.(i)) -. avg2 in
    num := !num +. (t1 *. t2);
    den1 := !den1 +. (t1 ** 2.0);
    den2 := !den2 +. (t2 ** 2.0);
  done;
  try !num /. ((sqrt !den1) *. (sqrt !den2))
  with
    Division_by_zero -> if den1=den2 then 1. else 0. (*in case v1 and/or v2 would be uniform*)

let reset vect value =
  for i = 0 to Array.length vect - 1 do
    vect.(i) <- value
  done

(* Interior and exterior? Iter post and pre?*)
let edge phi graph =
  let num_points = graph.num_nodes in
  let edgeset = ref PointsSet.empty in
  for point = 0 to num_points - 1 do
    let pp = phi point in
    graph.iter_post point
      (fun p w ->
	let pp' = phi p in
	let xor = pp +. pp' in
	if xor = 1.0 then
	  begin
	    if not (PointsSet.mem point !edgeset) then
	      edgeset := PointsSet.add point !edgeset;
	    if not (PointsSet.mem p !edgeset) then
	      edgeset := PointsSet.add p !edgeset;
	  end)
  done;
  !edgeset

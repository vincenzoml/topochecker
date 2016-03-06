let isTrue x = x = 1.0
let isFalse x = x = 0.0
let valTrue = 1.0
let valFalse = 0.0
let valUtil = 2.0
let valAnd x y = if x = 1.0 && y = 1.0 then 1.0 else 0.0
let valNot x = if x = 0.0 then 1.0 else 0.0
let ofBool x = if x then valTrue else valFalse
					
let debug s =
  Printf.eprintf "debug: %s\n%!" s		 
		 
let fail s =
  Printf.eprintf "%s\n%!" s;
  exit 1
    
let mkfname dir file =
  if Filename.is_relative file then dir ^ Filename.dir_sep ^ file else file	

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
    
let iter_neighbour dims i fn =
  let coords = coords_of_int i dims in
  for i = 0 to Array.length coords - 1 do
    let (orig,x,y) = (coords.(i),coords.(i) - 1,coords.(i) + 1) in
    if x >= 0 then 
      begin
	coords.(i) <- x;
	fn (int_of_coords coords dims);
      end;
    if y < dims.(i) then
      begin
	coords.(i) <- y;
	let id = int_of_coords coords dims in
	fn id;
      end;
    coords.(i) <- orig
  done

let assert_samelen a b = assert (Array.length a = Array.length b) 
    
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
		     
let statcmp bin1 bin2 thr = fail "Comparison of istograms not implemented"

let sqr x = x * x
				 
let euclidean_distance v1 v2 =
  assert_samelen v1 v2;
  let res = ref 0 in
  for i = 0 to Array.length v1 - 1 do
    res := !res + (sqr (v1.(i) - v2.(i)));
  done;
  sqrt (float_of_int !res)


       


       

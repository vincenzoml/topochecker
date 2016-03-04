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
    
									 

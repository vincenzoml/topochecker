type bus = int
type coord = { lat : float; long : float }
type position = { coord : coord; time : int; bus : bus }

type bin = { stops : coord list ref; buses : position list ref; idx : int }
module M = Map.Make(struct type t = bus let compare = compare end)
type busstate = { past : coord list; future : position list; delay : int; busid : int }
type systemstate = busstate M.t

let mtake1 l = match l with [] -> [] | x::xs -> [x]
let compare_sst st1 st2 =
  let filter_st (busno,bst) = (busno,mtake1 bst.past,bst.delay) in
  compare (List.map filter_st (M.bindings st1)) (List.map filter_st (M.bindings st2))
    
module N = Map.Make(struct type t = systemstate let compare = compare_sst end)
type 'a tree = { node : 'a;  next : (('a tree) ref) list }

let rec maxbranch tree =
  List.fold_left (fun acc treeref -> max (maxbranch !treeref) acc) (List.length tree.next) tree.next

let rec size tree =
  List.fold_left (fun acc treeref -> acc + (size !treeref)) 1 tree.next
    
let inbox coord (pos1,pos2) = coord.lat >= pos1.lat && coord.lat < pos2.lat && coord.long >= pos1.long && coord.long < pos2.long
let minutes hour minute second = ((int_of_string hour) * 60 * 60) + ((int_of_string minute) * 60) + (int_of_string second)
  
let load_buses filename day box =
  let chan = open_in filename in
  let result = ref [] in
  begin
    try
      while true do
        let line = input_line chan in
        let substr = Str.split (Str.regexp "[,:]") line in
        match substr with
          [busname;latitude;longitude;date;hour;minute;second;route] ->
	    let coord = { lat = float_of_string latitude;
			  long = float_of_string longitude } in
	    let bus = int_of_string busname in
	    if day = date then
	      if inbox coord box then
		result := { coord = coord;
			    time = minutes hour minute second; 
			    bus = bus } :: !result          
        | _ -> raise (Failure
			(Printf.sprintf
			   "Format not recognised in line %s, file %s" line filename))
      done;
    with
      End_of_file -> close_in chan
    | exn -> failwith (Printf.sprintf "a fatal exception occurred: %s"
			 (Printexc.to_string exn))
  end;
  !result

let load_stops filename box =
  let chan = open_in filename in
  let result = ref [] in
  begin
    try
      while true do
        let line = input_line chan in
        let substr = Str.split (Str.regexp "[,]") line in
        match substr with
          [latitude;longitude] ->
	    let coord = { lat = float_of_string latitude;
			  long = float_of_string longitude } in
	    if inbox coord box then result := coord :: !result
        | _ -> raise
	   (Failure
	      (Printf.sprintf "Format not recognised in line %s, file %s"
		 line filename))
      done;
    with
      End_of_file -> close_in chan
    | exn -> failwith (Printf.sprintf "a fatal exception occurred: %s"
			 (Printexc.to_string exn))
  end;
  List.rev !result

let load_box filename =
  let chan = open_in filename in
  let line1 = input_line chan in
  let line2 = input_line chan in
  close_in chan;
  match (List.map float_of_string (Str.split (Str.regexp ",") line1),
	 List.map float_of_string (Str.split (Str.regexp ",") line2))
  with ([minlat;minlong;maxlat;maxlong],[meterslat;meterslong]) ->
    let box = ({lat = minlat; long = minlong},
	       {lat = maxlat; long = maxlong}) in
    (box,meterslat,meterslong)
  | _ -> failwith (Printf.sprintf "wrong format in %s" filename)
     
let ((minbox,maxbox),meterslat,meterslong) = load_box "input/box.txt"
let (deltalat,deltalong) = (maxbox.lat -. minbox.lat,maxbox.long -. minbox.long) 
let (reslattarget,reslongtarget) = (10.0,10.0) 
let (nbinslatf,nbinslongf) = (max 1.0 (meterslat /. reslattarget),max 1.0 (meterslong/.reslongtarget)) 
let (nbinslat,nbinslong) = (int_of_float nbinslatf,int_of_float nbinslongf) 

let _ = Printf.printf "nbinslat: %d nbinslong: %d\n%!" nbinslat nbinslong
  
let meters coord =
  (((coord.lat -. minbox.lat) /. deltalat) *. meterslat,
   ((coord.long -. minbox.long) /. deltalong) *. meterslong)
    
let bins = Array.init (nbinslat * nbinslong)
  (fun idx -> { stops = ref []; buses = ref []; idx = idx }) 
         
let findbin coord =
  let (disclat,disclong) =
    (int_of_float (floor (((coord.lat -. minbox.lat) /. deltalat) *.
			     (float_of_int nbinslat))),
     int_of_float (floor (((coord.long -. minbox.long) /. deltalong) *.
			     (float_of_int nbinslong))))
  in
  (disclat + (disclong * nbinslat))      
  
(* Load stops *)
let stops = load_stops "output/stops.csv" (minbox,maxbox) 
 
(* Load buses *)
let buslst = load_buses "output/buses.csv" "2014-01-30" (minbox,maxbox) 

let buses =
  let h = Hashtbl.create 10 in
  List.iter (fun pos ->
      let r =
	try Hashtbl.find h pos.bus
	with _ -> (let r = ref [] in Hashtbl.add h pos.bus r; r)
      in
      r := pos::!r)
	    buslst;      
  let l = Hashtbl.fold
            (fun bus r l -> (bus,(List.sort (fun pos1 pos2 -> compare pos1.time pos2.time) !r))::l) h [] in
  let sorted = List.sort (fun (bus1,_) (bus2,_) -> compare bus1 bus2) l in
  List.fold_left (fun st (bus,future) -> M.add bus { past = []; future = future; delay = 0; busid = bus } st) M.empty sorted
  
(* Populate bins *)
(* (* do we need this? *) let _ = List.iter (fun pos -> let r = bins.(findbin pos.coord).buses in r := pos::!r) buslst *)
let _ = List.iter (fun coord -> let r = bins.(findbin coord).stops in r := coord::!r) stops 
  
let metricdist coord1 coord2 =
  let (m1x,m1y) = meters coord1 in
  let (m2x,m2y) = meters coord2 in
  sqrt (((m1x -. m2x)**2.) +. (m1y -. m2y)**2.)
      
let update : bus -> (busstate -> busstate) -> systemstate -> systemstate =
  fun bus fn st ->
    try
      M.add bus (fn (M.find bus st)) st
    with
      Not_found -> st
      
let map : (busstate -> busstate) -> systemstate -> systemstate = M.map

let mapl : (busstate -> 'a option) -> systemstate -> 'a list =
  fun fn st -> 
    List.rev (M.fold (fun busno bst acc -> match (fn bst) with Some x -> x::acc | None -> acc) st [])

let exists : (busstate -> bool) -> systemstate -> bool =
  fun fn st -> M.exists (fun busno bst -> fn bst) st
      
let rec splitwhile f (past,future) =
  match future with
    [] -> (past,[])
  | x::xs ->
     if f x
     then splitwhile f (x::past,xs)
     else (past,future)
       

let rec taken n list = (* first n elements *)
  if n <= 0 then []
  else match list with
    [] -> []
  | x::xs -> x::(taken (n-1) xs)

let rec sublist n m list = (* n-th to m-th element, boundaries included *)
  match list with
    [] -> []
  | x::xs ->
     if n <= 0
     then taken (m+1) list
     else sublist (n-1) (m-1) xs

let rec mklist n =
  if n < 0 then []
  else if n = 0 then [0]
  else n::(mklist (n-1))
    
let clumps : int -> int -> float -> coord list -> coord list -> bool =
  fun duration deltat deltas past1 past2 ->
    if past1 <> [] && past2 <> [] && !(bins.(findbin (List.hd past1)).stops) <> [] 
    then
      List.exists
	(fun deltat ->
	  let past1' = sublist 0 duration past1 in
	  let past2' = sublist deltat (duration + deltat) past2 in
	  if List.length past1' <> List.length past2'
	  then false
	  else 
            let l = List.combine past1' past2' in
	    List.for_all (fun (pos1,pos2) -> metricdist pos1 pos2 <= deltas) l)
	(mklist deltat)
    else false 

let avgpos : coord list -> coord =
  fun pl ->
    let (slat,slong,len) =
      List.fold_left
        (fun (slat,slong,len) pos -> (slat +. pos.lat,slong +. pos.long,len+.1.))
        (0.0,0.0,0.0)
        pl
    in
    { lat = slat /. len;
      long = slong /. len }

let replace : busstate -> systemstate -> systemstate =
  fun bst st -> M.add bst.busid bst (M.remove bst.busid st) 
      
let simstep : int -> int -> int -> int -> int -> float -> int -> systemstate -> systemstate list =
  fun time timestep waittime duration deltat deltas maxdelay state -> 
    let tmpstate =
      map
	(fun bst ->
	  let (p,f) =
	    splitwhile
	      (fun pos -> bst.delay + (pos.time / 60) < time + timestep)
	      ([],bst.future)
	  in
	  { bst with
	    past =
	      (match p with
		[] ->
		  (match bst.past with
		    [] -> []
		  | pos::posn -> pos::pos::posn )
	       | _ -> (avgpos (List.map (fun x -> x.coord) p))::bst.past);
	    future = f })
	state
    in
    tmpstate::
      (mapl
	 (fun bst ->
	   if bst.delay < maxdelay
	     && exists
	       (fun bst' ->
		 bst'.busid <> bst.busid &&
	           let t = clumps duration deltat deltas bst.past bst'.past in
		   (if t then let c = List.hd bst.past in
			      begin
				Printf.printf "time %d: bus %d clumps at (%f,%f) stops:" time bst.busid c.lat c.long;
				List.iter (fun pos -> Printf.printf "(%f,%f) " pos.lat pos.long) !(bins.(findbin (List.hd bst.past)).stops);
				Printf.printf "\n%!"
			      end);
		   t)
	       state
	   then Some (replace { bst with delay = bst.delay + waittime } tmpstate)
	   else None)
	 state)      

type parameters =
  { mintime : int; (* in minutes *)
    maxtime : int; (* in minutes *)
    timestep : int; (* in minutes *)
    waittime : int; (* in minutes *)
    duration : int; (* in timesteps *)
    deltat : int; (* in timesteps *)
    deltas : float; (* in meters *)
    maxdelay : int; (* in minutes *)
    init : systemstate; (* initial state *) }
      
let sim : parameters -> systemstate tree ref =
  (* duration, deltat are multiplied by timestep *)
  fun par ->
    let rec fn visited time state =
(*      try (* THIS IS NEEDED TO DETECT LOOPS; DO WE NEED THAT? *)
	let x = N.find state visited in Printf.printf "loop\n%!"; x
	with Not_found -> *)
      let r = ref { node = state; next = []} in
      let tree =
	{ node = state;
	  next =
	    if time <= par.maxtime
	    then
	      List.map
		(fn (N.add state r visited) (time + par.timestep))
		(simstep time par.timestep par.waittime par.duration par.deltat par.deltas par.maxdelay state)
	    else []}
      in
      r := tree;
      r
    in
    fn N.empty par.mintime par.init
      
let treeref = sim
  { mintime = 6 * 60 + 30; (* in minutes *)
    maxtime = 8 * 60 + 30; (* in minutes *)
    timestep = 1; (* in minutes *)
    waittime = 2; (* in minutes *)
    duration = 2; (* in timesteps *)
    deltat = 5; (* in timesteps *)
    deltas = 500.0; (* in meters *)
    maxdelay = 5; (* in minutes *)
    init = buses; (* initial state *) }

let _ = Printf.printf "max branching: %d size: %d\n%!" (maxbranch !treeref) (size !treeref)

let mksign (x,y) k colour img =
  for i = (max 0 (x-k)) to (min (img.Rgb24.width - 1) (x+k)) do
    for j = (max 0 (y-k)) to (min (img.Rgb24.height - 1) (y+k)) do
      Rgb24.set img i j colour
    done
  done

let load_image filename =
  match Bmp.load filename [] with
  | Images.Rgb24 rgbimg -> rgbimg
  |  _ -> failwith "Only RGB24 bmp images supported at the moment."

let findpixel img (minbox,maxbox) coord =
  (int_of_float ((coord.long -. minbox.long) /. (maxbox.long -. minbox.long) *. (float_of_int img.Rgb24.width)),
   img.Rgb24.height - (int_of_float ((coord.lat -. minbox.lat) /. (maxbox.lat -. minbox.lat) *. (float_of_int img.Rgb24.height))))

     
let save_image filename img =
  Bmp.save filename [] (Images.Rgb24 img)
  
let write_state sid nids kripkefile basename img colours systemstate =
  List.iter (fun nid -> Printf.fprintf kripkefile "%d->%d;\n%!" sid nid) nids;
  let img' = Rgb24.copy img in
  (M.iter (fun busid bst ->
    match bst.past with
      [] -> ()
    | coord::_ ->
       mksign (findpixel img' (minbox,maxbox) coord) 2 (colours busid) img')
     systemstate);
  save_image (Printf.sprintf "%s_%d.bmp" basename sid) img'

let stopscols k l =
  let (_,res) = List.fold_left (fun (n,res) coord -> (n+k,(coord,{Color.b = n; Color.r = 0; Color.g = 0})::res)) (0,[]) l in
  fun busid -> List.assoc busid res

let buscols k l =
  let (_,res) = List.fold_left (fun (n,res) (busid,_) -> (n+k,(busid,{Color.r = n; Color.b = 100; Color.g = 0})::res)) (0,[]) l in
  fun busid -> List.assoc busid res

    
let write_model basename imgfile tree =
  let genid = let cnt = ref 0 in
	      fun () -> let x = !cnt in cnt := x+1; x
  in
  let cols = buscols 15 (M.bindings tree.node) in
  Printf.printf "bindings: %d\n%!" (List.length (M.bindings tree.node));
  let rec write_model_rec sid kripkefile img tree =
    let rec getids next =
      match next with
	[] -> []
      | x::xs -> (genid())::(getids xs)
    in
    let nids = getids tree.next in
    write_state sid nids kripkefile basename img cols tree.node;  
    List.iter
      (fun (treeref,sid') -> write_model_rec sid' kripkefile img !treeref)
      (List.combine tree.next nids)
  in
  let kripkefile = open_out (Printf.sprintf "%s.dot" basename) in
  try
    Printf.fprintf kripkefile "digraph{\n%!";
    let img = load_image imgfile in
    (*     let cols = stopscols 10 stops in *)
    List.iter (fun coord -> mksign (findpixel img (minbox,maxbox) coord) 2 {Color.r = 0; Color.g=255; Color.b=0} img) stops;
    write_model_rec (genid()) kripkefile img tree;
    Printf.fprintf kripkefile "}\n%!";
    close_out kripkefile
  with exn -> 
    close_out kripkefile;
    failwith (Printf.sprintf "a fatal exception occurred: %s"
		(Printexc.to_string exn))
    
let _ =
  write_model "model/edinburgh" "input/map.bmp" !treeref

type coord = { lat : float; long : float }
type position = { coord : coord; time : int; bus : int }
type bin = { stops : coord list ref; buses : position list ref; idx : int }
type 'a tree = { node : 'a;  next : ('a tree) list }
  
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
	    if inbox coord box then 
              result := coord :: !result
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
let (reslattarget,reslongtarget) = (5.0,5.0) 
let (nbinslatf,nbinslongf) = (meterslat /. reslattarget,meterslong/.reslongtarget) 
let (nbinslat,nbinslong) = (int_of_float nbinslatf,int_of_float nbinslongf) 

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
  List.sort (fun (bus1,_) (bus2,_) -> compare bus1 bus2) l	
        
(* Populate bins *)
let _ =
  List.iter (fun pos -> let r = bins.(findbin pos.coord).buses in r := pos::!r) buslst; (* do we need this? *)
  List.iter (fun coord -> let r = bins.(findbin coord).stops in r := coord::!r) stops;
  
let metricdist coord1 coord2 =
  let (m1x,m1y) = meters coord1 in
  let (m2x,m2y) = meters coord2 in
  sqrt (((m1x -. m2x)**2.) +. (m1y -. m2y)**2.)
    
type bus = int
    
module M = Map.Make (bus)
    
type busstate = { past : coord list; future : pos list; delay : int }

type systemstate = busstate M.t
  
let update : bus -> (busstate -> busstate) -> systemstate -> systemstate =
  fun bus -> fn -> st ->
    try
      M.add st bus (fn (M.find bus))
    with
      Not_found -> st

let mapa : (busstate -> 'a) -> systemstate -> 'a list =  
  fun fn st ->
    let r = ref [] in
    M.iter (fun busstate -> r := (fn busstate)::!r) st;
    List.rev !r
      
let map : (busstate -> busstate) -> systemstate -> systemstate = M.map      
	
let rec splitwhile f (past,future) =
  match future with
    [] -> (past,[])
  | x::xs ->
     if f x
     then split f (x::past,xs)
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
       
let clumps : int -> int -> float -> coord list -> coord list -> bool =
  fun duration deltat deltas delay1 past1 delay2 past2 ->
    if past1 <> [] && past2 <> [] && !(bins.(findbin (List.head past1)).stops) <> []
    then
      let past1' = sublist 0 duration past1 in
      let past2' = sublist deltat (duration + deltat) past2 in
      let l = List.combine past1' past2' in
      if List.length l <> duration then Printf.printf "debug: notgood\n%!";
      List.length l = duration && 
	  List.forall (fun (pos1,pos2) -> metricdist pos1 pos2 <= deltas) l
    else false

let avgpos : position list -> coord =
  fun pl ->
    assert (pl <> []);
    let bus = (List.head pl).bus in
    let (slat,slong,stime,len) = List.fold_left
      (fun (slat,slong,len) pos -> (slat +. coord.pos.lat,slong +. coord.pos.long,len+.1)) (0.0,0.0,0.0) in
    { lat = slat /. len;
      long = slong /. len }
	
let simstep : int -> int -> int -> int -> int -> float -> systemstate -> systemstate list =
  fun time timestep waittime duration deltat deltas state ->    
    let tmpstate =
      map
	(fun bst ->
	  let (p,f) =
	    splitwhile
	      (fun pos -> bst.delay + (pos.time / 60) < time + timestep)
	      bst.future
	  in
	  { bus with
	    past =
	      (match p with
		[] ->
		  (match bus.past with
		    [] -> []
		  | pos::posn -> pos::pos::posn )
	      | _ -> (avgpos p)::bst.past);
	    future = f })
	state
    in
    tmpstate::(filtermap
	(fun bst ->
	  if List.exists
	    (fun bst' -> bst'.bus <> bst.bus &&
	      clumps duration deltat deltas bst.past bst'.past)
	    state
	  then { bst with delay = delay + waittime }
	  else bst)
	state)

let sim : int -> int -> int -> int -> int -> int -> float -> systemstate -> systemstate tree =
  fun maxtime timestep waittime duration deltat deltas init ->
    let rec fn time state =
	{ node = state;
	  next = if time < maxtime then List.map (fn (time + timestep)) (simstep time timestep waittime duration deltat deltas state) else []}
    in
    fn 0 init          
    
(* let _ = sim (24 * 60) 1 5 3 1 10.0 (mkinit buses) *)

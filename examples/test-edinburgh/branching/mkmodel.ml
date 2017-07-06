type coord = { lat : float; long : float }
type position = { coord : coord; time : int; bus : int }
type bin = { stops : coord list ref; buses : position list ref; idx : int }
(* type result = { step : int; status : position list; next : result list }   *)

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
let (reslattarget,reslongtarget) = (20.0,20.0) 
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
  List.iter (fun coord -> let r = bins.(findbin coord).stops in r := coord::!r) stops

let rec select time delay deltat path acc =
  match path with
    [] -> List.rev acc
  | pos::posn ->
     if pos.time + delay >= time + deltat
     then acc
     else select time delay deltat posn (pos::acc)

let metricdist coord1 coord2 =
  let (m1x,m1y) = meters coord1 in
  let (m2x,m2y) = meters coord2 in
  sqrt (((m1x -. m2x)**2.) +. (m1y -. m2y)**2.)
    
let bumpsinto bins time deltat deltas (bus1,path1,delay1) (bus2,path2,delay2) =
  match (path1,path2) with
    (([],_)|(_,[])) -> false
  | (pos1::posn1,pos2::posn2) ->
     !(bins.(findbin pos1.coord).stops) <> [] &&
     bus1 <> bus2 &&
       pos1.time + delay1 >= time &&
       pos1.time + delay1 < time + deltat &&
       List.exists (fun pos -> (metricdist pos1.coord pos.coord) < deltas)
       (select time delay2 deltat path2 [])       
       
let mkresult timestep waittime deltat deltas buses =
  let rec fn time busdelays =
    if List.for_all (fun ((bus,path),delay) -> path = []) busdelays
    then { node = (time,busdelays); next = [] }
    else
       let partials =
	 List.concat 
	   (List.map
	      (fun ((bus,path),delay) ->
		match path with
		  [] -> []
		| pos::posn ->
		   if pos.time + delay < (time * timestep)
		   then
		     (if List.exists
			 (fun ((bus',path'),delay') ->
			   bumpsinto bins time deltat deltas
			     (bus,path,delay) (bus',path',delay'))		   
			busdelays
		      then [((bus,posn),delay);((bus,posn),delay+waittime)]
		      else [((bus,posn),delay)])
		   else [((bus,path),delay)])
	      busdelays)
       in
       { node = (time,busdelays);
	 next =
	   let futures =
	     List.map
	       (fun ((bus,path),delay) ->
		 List.map (fun ((bus',path'),delay') ->
		   if bus = bus'
		   then ((bus,path),delay)
		   else ((bus',path'),delay'))
		   busdelays)
	       partials
	   in	  
	   List.map (fn (time + 1)) futures }
  in      
  fn 0 (List.map (fun (bus,path) -> ((bus,path),0)) buses)
  
let _ = mkresult 60 180 60 10.0 buses 








































    
(*
  let paths =
  List.map
  (fun (bus,poss) ->
  { busno = bus;
  delay = 0;
  steps = 
  List.rev
  (List.fold_left
  (fun path pos ->
  { bin = findbin pos.coord;
  stime = bus.time}::path) [] poss) })
	buses
  in
*)
    


(*
    let indeltat t deltat path =
      let rec fn steps acc =
	match steps with
	  [] -> List.rev acc
	| step::steps' ->
	   if path.delay + step.stime > (t+deltat)
	   then List.rev acc
	   else fn steps' (step::acc)
      in
    
    let generateModel paths step delay deltat =
      let tstart =
	List.fold_left
	  (fun m (bus,steps) ->
	    match steps with
	      [] -> m
	    | x::xs -> min (x.stime + x.delay) m) buses in

      let concatmap fn l = List.concat (List.map fn l) in

      let rec core t step deltat delay choices =
	let choices' =
	  concatmap 
	    (fun paths ->
	       let choices' =
		 List.map
		   (fun path ->
		     if (path.steps <> []) &&
		       (List.exists
			  (fun path' ->
			    (path'.busno <> path.busno) && (has_conflict path path' t ))
			  paths)
		     then [choices;
		     )
		   choices)
	     choices)
	in if newchoices = [] then choices else core t+step deltat newchoices
      in
      core 0 step deltat [paths]
	
*)

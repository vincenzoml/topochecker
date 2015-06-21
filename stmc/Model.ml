open StlLogic

(** Segnatura dello spazio **)
module type SPACE = sig

  type t
  type point
  type pointset

  val domain : t -> pointset
  val empty : pointset

  val string_of_point : point -> string
  val fold : (point -> 'a -> 'a) -> pointset -> 'a -> 'a

  val mem : point -> pointset -> bool
  val choose : pointset -> point
  val add : point -> pointset -> pointset
  val singleton : point -> pointset
  val diff : pointset -> pointset -> pointset
  val subset : pointset -> pointset -> bool
  val inter : pointset -> pointset -> pointset
  val union : pointset -> pointset -> pointset
  val complement : pointset -> t -> pointset
  val iter : (point -> unit) -> pointset -> unit
  val filter : (point -> bool) -> pointset -> pointset
  val compare : point -> point -> int

  val pred : point -> t -> pointset
  val next : point -> t -> pointset
  val closure : pointset -> t -> pointset

end

(** Segnatura del tempo **)
module type TIME = sig

  type t
  type point
  type pointset
  val domain : t -> pointset
  val empty : pointset

  val string_of_point : point -> string

  val mem : point -> pointset -> bool
  val singleton : point -> pointset
  val add : point -> pointset -> pointset
  val subset : pointset -> pointset -> bool
  val inter : pointset -> pointset -> pointset
  val union : pointset -> pointset -> pointset
  val diff : pointset -> pointset -> pointset
  val choose : pointset -> point
  val complement : pointset -> t -> pointset
  val remove : point -> pointset -> pointset
  val filter : (point -> bool) -> pointset -> pointset
  val iter : (point -> unit) -> pointset -> unit
  val fold : (point -> 'a -> 'a) -> pointset -> 'a -> 'a
  val compare : point -> point -> int
  
  val pred : point -> t -> pointset
  val next : point -> t -> pointset
  

end






(** Implementazione del modello **)
module Model (Space : SPACE) (Time : TIME) : (MODEL with type space = Space.t
						     and type space_point = Space.point
						     and type space_pointset = Space.pointset
						     and type time = Time.t
						     and type time_point = Time.point
						    and type time_pointset = Time.pointset ) = struct


  (** Spazio **)
  type space = Space.t
  type space_point = Space.point
  type space_pointset = Space.pointset
  let space_domain = Space.domain
  let space_empty = Space.empty

  module SpacePoint = struct
    type t = space_point
    let compare = Space.compare
  end

  let string_of_space_point = fun sp -> Space.string_of_point sp
  let string_of_space_pointset = fun spset ->
    Printf.sprintf "[ %s ]" (String.concat " " (Space.fold (fun x l -> (string_of_space_point x)::l) spset [] ) )

  let space_mem = fun sp spset -> Space.mem sp spset
  let space_add = Space.add
  let space_singleton = Space.singleton
  let space_choose = Space.choose
  let space_diff = Space.diff
  let space_subset = fun spset1 spset2 -> Space.subset spset1 spset2
  let space_inter = fun spset1 spset2 -> Space.inter spset1 spset2
  let space_union = fun spset1 spset2 -> Space.union spset1 spset2
  let space_complement = fun domain spset -> Space.complement domain spset
  let space_filter = fun flt spset -> Space.filter flt spset
  let space_iter = Space.iter
  let space_fold = Space.fold
  
  let space_pred = Space.pred
  let space_next = Space.next
  let space_closure = Space.closure




  (** Tempo **)
  type time = Time.t
  type time_point = Time.point
  type time_pointset = Time.pointset
  let time_domain = Time.domain
  let time_empty = Time.empty

  module TimePoint = struct
    type t = time_point
    let compare = Time.compare
  end

  let string_of_time_point = fun tp -> Time.string_of_point tp
  let string_of_time_pointset = fun tpset ->
    Printf.sprintf "[ %s ]" (String.concat " " (Time.fold (fun x l -> (string_of_time_point x)::l) tpset [] ) )

  let time_mem = fun tp tpset -> Time.mem tp tpset
  let time_singleton = Time.singleton
  let time_add = fun tp tpset -> Time.add tp tpset
  let time_subset = fun tpset1 tpset2 -> Time.subset tpset1 tpset2
  let time_inter = fun tpset1 tpset2 -> Time.inter tpset1 tpset2
  let time_union = fun tpset1 tpset2 -> Time.union tpset1 tpset2
  let time_diff = fun tpset1 tpset2 -> Time.diff tpset1 tpset2
  let time_choose = Time.choose
  let time_complement = fun domain tpset -> Time.complement domain tpset
  let time_remove = Time.remove
  let time_filter = fun flt tpset -> Time.filter flt tpset
  let time_iter = Time.iter
  let time_fold = fun flt tpset a -> Time.fold flt tpset a

  let time_pred = fun tp domain -> Time.pred tp domain
  let time_next = fun tp domain -> Time.next tp domain

  

  (** Spazio-Tempo **)
  type st = space * time

  let st_make = fun sp ti -> (sp,ti)

  type st_point = space_point * time_point
  let st_compare = fun stpt1 stpt2 ->
    let (spt1,tpt1) = stpt1 in
    let (spt2,tpt2) = stpt2 in
    let comp1 = Space.compare spt1 spt2 in
    if comp1 = 0
    then Time.compare tpt1 tpt2
    else comp1
      
  module StPoint = struct
    type t = st_point
    let compare = st_compare
  end
  module StSet = Set.Make(StPoint)

  type st_pointset = StSet.t




  let string_of_st_point = fun stpt ->
    let (spt,tpt) = stpt in
    Printf.sprintf "(%s;%s)" (string_of_space_point spt) (string_of_time_point tpt)




  let st_cardinal = StSet.cardinal
  let st_mem = StSet.mem
  let st_choose = StSet.choose
  let st_add = StSet.add
  let st_remove = StSet.remove
  let st_subset = StSet.subset
  let st_diff = StSet.diff
  let st_inter = StSet.inter
  let st_union = StSet.union
  let st_iter = StSet.iter
  let st_filter = StSet.filter
  let st_fold = StSet.fold



  let st_empty = StSet.empty
  let st_domain = fun st ->
    let (space,time) = st in
    let sdom = space_domain space in
    let tdom = time_domain time in
    let acc = ref(st_empty) in
    let add_stpoint = fun x t -> acc := st_add (x,t) (!acc) in
    let add_spoint = fun x -> time_iter (add_stpoint x) tdom in
    let _ = space_iter add_spoint sdom in
    !acc
 
  let st_complement = fun stset st -> st_diff (st_domain st) stset

  let st_make_point = fun s t -> (s,t)

  (* let st_space_section = fun tp stset -> *)
  (*   let win_points = st_filter (fun (s,t) -> (t=tp)) stset in *)
  (*   st_fold (fun (s,t) sset -> space_add s sset) win_points space_empty *)
  module TimeMap = Map.Make(TimePoint)

  let st_space_section = fun stset ->
    let map = ref TimeMap.empty in
    let add_point = fun stp ->
      let (sp,tp) = stp in
      try map := TimeMap.add tp (space_add sp (TimeMap.find tp (!map))) (!map)
      with Not_found ->
	map := TimeMap.add tp (space_singleton sp) (!map)
    in
    let _ = st_iter add_point stset in
    fun tp -> if TimeMap.mem tp (!map) then TimeMap.find tp (!map) else space_empty

  (* let st_time_section = fun sp stset -> *)
  (*   let win_points = st_filter (fun (s,t) -> (s=sp)) stset in *)
  (*   st_fold (fun (s,t) tset -> time_add t tset) win_points time_empty *)
  module SpaceMap = Map.Make(SpacePoint)

  let st_time_section = fun stset ->
    let map = ref SpaceMap.empty in
    let add_point = fun stp ->
      let (sp,tp) = stp in
      try map := SpaceMap.add sp (time_add tp (SpaceMap.find sp (!map))) (!map)
      with Not_found ->
	map := SpaceMap.add sp (time_singleton tp) (!map)
    in
    let _ = st_iter add_point stset in
    fun sp -> if SpaceMap.mem sp (!map) then SpaceMap.find sp (!map) else time_empty

  let st_cartesian_product = fun sset tset ->
    let space_fix = fun sp -> time_fold (fun t stpset -> st_add (sp,t) stpset) tset st_empty in
    space_fold (fun s stpset -> st_union (space_fix s) stpset) sset st_empty



  let string_of_st_pointset_aux = fun stset ->
    let tset = st_fold (fun (s,t) ts -> time_add t ts) stset time_empty in
    let space_sec = st_space_section stset in
    let string_time_fix = fun t -> Printf.sprintf "%s %s\n" (string_of_time_point t) (string_of_space_pointset (space_sec t)) in
    time_fold (fun t str -> Printf.sprintf "%s%s" (string_time_fix t) str) tset ""
  let string_of_st_pointset = fun stset ->
    Printf.sprintf "{ %s }" (string_of_st_pointset_aux stset)




  let st_space_closure = fun stps st ->
    let (space,time) = st in
    let tdom = time_domain time in
    let space_sec = st_space_section stps in
    (* let sp_closure = fun sset -> space_closure sset space in *)
    (* let smart_closure = fun t fset -> st_union fset (st_cartesian_product (sp_closure (st_space_section t stps)) (time_singleton t)) in *)
    (* time_fold smart_closure tdom st_empty *)
    let t_to_spset = fun t -> st_cartesian_product (space_closure (space_sec t) space) (time_singleton t) in
    let smart_fold = fun t stset -> st_union (t_to_spset t) stset in
    time_fold smart_fold tdom st_empty
    

  let st_time_pred = fun stpt st ->
    let (space,time) = st in
    let tpred = fun t -> time_pred t time in
    let (s,t) = stpt in
    st_cartesian_product (space_singleton s) (tpred t)

  let st_time_next = fun stpt st ->
    let (space,time) = st in
    let tnext = fun t -> time_next t time in
    let (s,t) = stpt in
    st_cartesian_product (space_singleton s) (tnext t)
 


  let st_space = fun st -> let (s,t) = st in s
  let st_time = fun st -> let (s,t) = st in t

  let st_to_space = fun stpt ->
    let (spt,tpt) = stpt in spt

  let st_to_time = fun stpt ->
    let (spt,tpt) = stpt in tpt

  

  (* let add_stpt = fun sp tp stpset -> *)
  (*   try *)
  (*     let tpset = time_add tp (StMap.find sp stpset) in *)
  (*     StMap.add sp tpset stpset *)
  (*   with Not_found -> *)
  (*     StMap.add sp (time_add tp time_empty) stpset *)



end






(*** TEST AREA!!! DA CANCELLARE!!! ***)


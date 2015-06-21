(* segnatura dei punti del grafo *)
module type POINT = sig
  type t
  val string_of_point : t -> string
  val compare : t -> t -> int
end


(** Definisco i grafi quasi-discrete (cioè dotati di un operatore di chiusura topologica), che sono le strutture della logica presa in esame **)
(* segnatura *)
module type QDGRAPH = sig
    
  type t
  type point
  type pointset
  
  val empty : t
  val emptyset : pointset
    
  val string_of_point : point -> string
    
  val mem : point -> pointset -> bool
  val singleton : point -> pointset
  val subset : pointset -> pointset -> bool
  val add : point -> pointset -> pointset
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
    
  val set_nodes : pointset -> t -> t
  val set_source : (point -> pointset) -> t -> t
  val set_destination : (point -> pointset) -> t -> t
  val set_closure : (pointset -> pointset) -> t -> t

  val add_node : point -> t -> t
  val add_arc : point -> point -> t -> t
  val add_edge : point -> point -> t -> t
  val standard_closure : t -> t
    
  val get_nodes : t -> pointset
  val get_source : t -> point -> pointset
  val get_destination : t -> point -> pointset
  val get_closure : t -> pointset -> pointset
    
end

(* Un QDGraph è un insieme di nodi collegati tra loro da archi, più una relazione di chiusura che ad un sottoinsieme associa la chiusura rispetto dello stesso; i nodi sono punti *)
module QDGraph (Point : POINT) : (QDGRAPH with type point = Point.t) = struct


  (** error handling **)
    


  (** definisco il tipo dei punti e degli insiemi di punti **)
  module PSet = Set.Make(Point)
  type pointset = PSet.t
  type point = PSet.elt

  (* il tipo dei qdgraph *)
  type t = {
    nodes : pointset;
    source : point -> pointset;
    destination : point -> pointset;
    closure : pointset -> pointset;
  }

  (* il grafo vuoto *)
  let empty = {
    nodes = PSet.empty;
    (* restituisce gli archi di cui x è sorgente *)
    source = (fun x -> PSet.empty);
    (* restituisce gli archi di cui x è destinazione *)
    destination = (fun x -> PSet.empty);
    closure = (fun x -> PSet.empty)
  }

  (* l'insieme di punti vuoto *)
  let emptyset = PSet.empty

  (* il dominio del grafo *)
  let domain = empty



  (* operazioni derivate *)
  let compare = Point.compare

  let mem = PSet.mem
  let singleton = PSet.singleton
  let add = PSet.add
  let subset = PSet.subset
  let inter = PSet.inter
  let union = PSet.union
  let diff = PSet.diff
  let choose = PSet.choose
  let complement = fun ps domain -> diff domain.nodes ps
  let remove = PSet.remove
  let filter = PSet.filter
  let iter = PSet.iter
  let fold = PSet.fold




  
  (** funzioni di conversione a stringa **)
  let string_of_point = Point.string_of_point

  let string_of_pointset = fun pset ->
    let nodes_list = PSet.elements pset in
    List.map Point.string_of_point nodes_list

  (* da nodi a stringhe - restituisce una lista contenente (string_of_point x) al variare di x tra i nodi *)
  let string_of_nodes = fun qdgraph -> string_of_pointset qdgraph.nodes

  let string_of_source = fun qdgraph ->
    let nodes_list = PSet.elements qdgraph.nodes in
    let src_list = List.map qdgraph.source nodes_list in
    List.map string_of_pointset src_list
   
  let string_of_destination = fun qdgraph ->
    let nodes_list = PSet.elements qdgraph.nodes in
    let dst_list = List.map qdgraph.destination nodes_list in
    List.map string_of_pointset dst_list

  (* stampa la chiusura di un certo sottoinsieme dei nodi *)
  let string_of_closure = fun sset qdgraph ->
    let sset_cls = qdgraph.closure sset in
    string_of_pointset sset_cls







  (** le seguenti funzioni restituiscono il contenuto di un grafo **)
  (* Per ora le metto per debug. Le dovrei cancellare? *)
  let get_nodes = fun gr -> gr.nodes
  let get_source = fun gr -> gr.source
  let get_destination = fun gr -> gr.destination
  let get_closure = fun gr -> gr.closure






  (** operazioni sui grafi **)
  (* fatta veramente male, ha un costo computazionale enorme *)
  (* Quando aggiungo un punto al grafo lo considero un punto isolato *)
  let add_node = fun pt gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    if PSet.mem pt nd
    then gr
    else
      {
	nodes = (PSet.add pt nd);
	source = (fun x -> if x=pt then PSet.empty else src x);
	destination = (fun x -> if x=pt then PSet.empty else dst x);
	closure = (fun x -> if (PSet.mem pt x) then (PSet.add pt (cls x)) else cls x);
      }

  (* aggiungo un arco; da usare se il grafo è orientato *)
  let add_arc = fun pts ptd gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    {
      nodes = nd;
      source = (fun x -> if x=ptd then PSet.add pts (src ptd) else src x);
      destination = (fun x -> if x=pts then PSet.add ptd (dst pts) else dst x);
      closure = (fun x -> cls x);
    }

  (* aggiungo un lato; da usare se il grafo è non orientato *)
  let add_edge = fun pts ptd gr ->
    let gr1 = add_arc pts ptd gr in
    add_arc ptd pts gr1

  (**)
  let set_nodes = fun nnd gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    {nodes=nnd;source=src;destination=dst;closure=cls}

  let set_source = fun nsrc gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    {nodes=nd;source=nsrc;destination=dst;closure=cls}
  
  let set_destination = fun ndst gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    {nodes=nd;source=src;destination=ndst;closure=cls}

  (* definisce la chiusura di un insieme *)
  let set_closure = fun ncls gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    {nodes=nd;source=src;destination=dst;closure=ncls}

  (* definisci la chiusura di un insieme *)
  (* let define_closure = fun sset sset_cls gr -> *)
  (*   let {nodes=nd;source=src;destination=dst;closure=cls} = gr in *)
  (*   { *)
  (*     nodes = nd; *)
  (*     source = src; *)
  (*     destination = dst; *)
  (*     closure = fun x -> if x=sset then sset_cls else cls x *)
  (*   } *)



  (** la seguente funzione serve a definire la chiusura standard di un grafo (quella indotta dalla relazione "essere estremi di un arco") **)
  let rec standard_closure = fun gr ->
    let {nodes=nd;source=src;destination=dst;closure=cls} = gr in
    let point_closure = fun p -> PSet.add p (dst p) in
    let smart_fold = fun p pset -> PSet.union (point_closure p) pset in
    let ncls = fun pset -> PSet.fold smart_fold pset PSet.empty in
    (* let ncls = (fun p -> PSet.fold (fun el res -> PSet.union (dst el) res) p p) in *)
    {nodes=nd;source=src;destination=dst;closure=ncls}
       





end


















(*** ZONA TEST!!! DA CANCELLARE!!! ***)

(* module Point = struct *)
(*   type t = string *)
(*   let string_of_point x = x *)
(*   let compare x y = String.compare x y *)
(* end *)

(* module Grafo = QDGraph(Point) *)

(* let empty = Grafo.empty *)

(* let prova = Grafo.add_node "ciccio" empty *)
(* let prova = Grafo.add_node "pippo" prova *)
(* let prova = Grafo.add_arc "ciccio" "pippo" prova *)
(* let prova = Grafo.add_arc "pippo" "ciccio" prova *)
(* let prova = Grafo.define_closure (Grafo.get_nodes prova) (Grafo.get_nodes prova) prova *)


(* (\* esempio nisekoi *\) *)
(* let nisekoi = Grafo.add_node "raiku" empty *)
(* let nisekoi = Grafo.add_node "chitoge" nisekoi *)
(* let nisekoi = Grafo.add_node "onodera" nisekoi *)
(* let nisekoi = Grafo.add_node "ruri" nisekoi *)

(* let nisekoi = Grafo.add_edge "raiku" "chitoge" nisekoi *)
(* let nisekoi = Grafo.add_edge "raiku" "onodera" nisekoi *)
(* let nisekoi = Grafo.add_edge "onodera" "ruri" nisekoi *)

(* let nisekoi = Grafo.standard_closure nisekoi *)

open Sys
open Images
open Graphics
open Bmp
open Bitmap
open Graphic_image
open Thread
open StlLogic
open Graph
open Model
open StlConvert

(** spazio **)

module MySpacePoint = struct
  type t = int * int
  let string_of_point = fun (x,y) -> Printf.sprintf "(%d,%d)" x y
  let compare = fun (x1,y1) (x2,y2) ->
    if Pervasives.compare x1 x2 = 0
    then Pervasives.compare y1 y2
    else Pervasives.compare x1 x2
end
module MySpaceGraph = QDGraph(MySpacePoint)
module MySpace = SpaceOfQDGraph(MySpaceGraph)

(** tempo **)

module MyTimePoint = struct
  type t = int
  let string_of_point = fun x -> string_of_int x
  let compare = Pervasives.compare
end
module MyTimeGraph = QDGraph(MyTimePoint)
module MyTime = TimeOfQDGraph(MyTimeGraph)

(** modello **)
module MyModel = Model(MySpace)(MyTime)

(** proposizioni **)
module MyProp = struct
  type t =
  | RedRange of int * int
  | GreenRange of int * int
  | BlueRange of int * int
  | Id of string
  let string_of = fun pr ->
    match pr with
    | RedRange (x,y) -> Printf.sprintf "RED [%d,%d]" x y
    | GreenRange (x,y) -> Printf.sprintf "GREEN [%d,%d]" x y
    | BlueRange (x,y) -> Printf.sprintf "BLUE [%d,%d]" x y
    | Id s -> s
  type t_sem = MyModel.st_pointset
  let string_of_sem = fun ps -> "pset"
  let compare = fun t1 t2 ->
    match (t1,t2) with
    | (RedRange(x1,y1),RedRange(x2,y2)) -> if x1>x2 then x1-x2 else y1-y2
    | (RedRange(x1,y1),_) -> 1
    | (_,RedRange(x1,y1)) -> -1
    | (BlueRange(x1,y1),BlueRange(x2,y2)) -> if x1>x2 then x1-x2 else y1-y2
    | (BlueRange(x1,y1),_) -> 1
    | (_,BlueRange(x1,y1)) -> -1
    | (GreenRange(x1,y1),GreenRange(x2,y2)) -> if x1>x2 then x1-x2 else y1-y2
    | (GreenRange(x1,y1),_) -> 1
    | (_,GreenRange(x1,y1)) -> -1
    | (Id(s1),Id(s2)) -> String.compare s1 s2
  type env = t -> t_sem
  let empty_env = fun pr -> failwith "L'ambiente non è definito su questo input"
  let bind = fun pr stset en ->
    match pr with
    | Id s ->
      (fun npr -> match npr with
      | Id t -> if s=t then stset else en (Id t)
      | x -> en x)
    | _ -> failwith "modifica non consentita"
end

(** logica **)
module MyLogic = Logic(MyModel)(MyProp)




(** comandi **)

type 'a command =
| SHOW_STATUS
| SHOW_FORMULA
| SHOW_STORE
| SHOW_FUTURE
| SHOW_SPACE of Graphics.color
| SHOW_TIME
| SET_TIME of int
| SET_SPACE of int * int
| LET of string * string list * 'a
| SEM_IDE of Graphics.color * string * string list
| SEM of Graphics.color * (MyModel.st_pointset MyLogic.fsyntax)
| SAVE_STORE
| LOAD_STORE
| SAVE_IMAGE of string
| SAVE_SINGLE_IMAGE of int * string
| RESET
| REFRESH
| STOP_TEST




(*** comandi per la grafica ***)

(** funzioni per i colori **)
let red color = (color / (256 * 256)) mod 256 
let green color = (color / 256) mod 256 
let blue color = color mod 256 

(* val color_to_rgb : Graphics.color -> Color.rgb.t *)
let color_to_rgb = fun color ->
  { r = red color;
    g = green color;
    b = blue color
  }

(* val string_of_color : Color.t -> string *)
let string_of_color rgb = Printf.sprintf "#%02X%02X%02X" rgb.r rgb.g rgb.b

(* val over : Color.rgb -> Color.rgb -> Color.rgb *)
let over c1 c2 =
  let factor = 0.7 in
  let value x y = int_of_float (((float_of_int x) *. factor) +. ((float_of_int y) *. (1.0 -. factor))) in
  { r = value c1.r c2.r;
    g = value c1.g c2.g;
    b = value c1.b c2.b; }

(** funzioni per la grafica **)
(* val cross_of_point : MyModel.st_point -> MyModel.t -> MyModel.st_pointset *)
let cross_of_point = fun st model ->
  let set = ref MyModel.st_empty in
  for i = 1 to 15 do
    set := MyModel.st_space_closure (!set) model
  done;
  !set

(** Conversione delle coordinate **)
(* val xyspace_to_xyimg : Rgb24.t -> (int * int) -> int * int *)
let xyspace_to_xyimage = fun img (x,y) ->
  let (oldx,oldy) = (float_of_int x,float_of_int y) in
  let (ximg,yimg) = (float_of_int img.Rgb24.width,float_of_int img.Rgb24.height) in
  let (xgraphic,ygraphic) = (float_of_int (Graphics.size_x()),float_of_int (Graphics.size_y())) in
  ( int_of_float(( oldx /. ximg ) *. xgraphic) , int_of_float (( oldy /. yimg ) *. ygraphic) )

(* val xyimage_to_xyspace : Rgb24.t -> (int * int) -> int * int *)
let xyimage_to_xyspace = fun img (x,y) ->
  let (oldx,oldy) = (float_of_int x,float_of_int y) in
  let (ximg,yimg) = (float_of_int img.Rgb24.width,float_of_int img.Rgb24.height) in
  let (xgraphic,ygraphic) = (float_of_int (Graphics.size_x()),float_of_int (Graphics.size_y())) in
  ( int_of_float(( oldx /. xgraphic ) *. ximg) , int_of_float (( oldy /. ygraphic ) *. yimg) )

(** funzioni di salvataggio e caricamento immagini **)
(* val load_image : string -> Rgb24.t *)
let load_image filename =
  match Bmp.load filename [] with
  | Rgb24 rgbimg -> rgbimg
  |  _ -> failwith "Only RGB24 bmp images supported at the moment."

(* val save_image : Rgb24.t -> string -> unit *)
let save_image img filename =
  Bmp.save filename [] (Rgb24 img)


(** modello da immagine **)
(* val digital_subspace : (int * int) -> (int * int) -> MySpaceGraph.pointset *)
let rec digital_subspace (xs,ys) (xe,ye) =
  let init = ref MySpaceGraph.emptyset in
  for y = ys to ye do
    for x = xs to xe do
      init := MySpaceGraph.add (x,y) (!init)
    done
  done;
  !init

(* val set_of_list : MySpaceGraph.point list -> MySpaceGraph.pointset *)
let rec set_of_list l =
  match l with
    [] -> MySpaceGraph.emptyset
  | x::xs -> MySpaceGraph.add x (set_of_list xs)

(* (\* val space_of_image : Rgb24.t -> (MySpaceGraph.t * (MyProp.t -> MySpaceGraph.pointset)) *\) *)
(* let space_of_image rgbimg = *)
(*   let points =  digital_subspace (0,0) (rgbimg.Rgb24.width - 1,rgbimg.Rgb24.height - 1) in *)
(*   let neighbours (x,y) = *)
(*     set_of_list *)
(*       (List.filter *)
(* 	 (fun (a,b) -> a >= 0 && a < rgbimg.Rgb24.width && b >= 0 && b < rgbimg.Rgb24.height) *)
(* 	 [(x-1,y-1);(x-1,y);(x-1,y+1);(x,y-1);(x,y+1);(x+1,y-1);(x+1,y);(x+1,y+1)]) in *)
(*   let clos = (fun p -> MySpaceGraph.fold (fun el res -> MySpaceGraph.union (neighbours el) res) p p) in *)
(*   let clr_range = fun pr (x,y) -> *)
(*     match pr with *)
(*     | MyProp.RedRange (rd,ru) -> let rxy = (Rgb24.get rgbimg x y).r in rd<=rxy && rxy<=ru *)
(*     | MyProp.GreenRange (gd,gu) -> let gxy = (Rgb24.get rgbimg x y).g in gd<=gxy && gxy<=gu *)
(*     | MyProp.BlueRange (bd,bu) -> let bxy = (Rgb24.get rgbimg x y).b in bd<=bxy && bxy<=bu *)
(*     | _ -> MyProp.empty_env pr *)
(*   in *)
(*   ( MySpaceGraph.set_nodes points *)
(*       (MySpaceGraph.set_source neighbours *)
(* 	 (MySpaceGraph.set_destination neighbours *)
(* 	    (MySpaceGraph.set_closure clos (MySpaceGraph.empty)))) , *)
(*     fun pr -> MySpaceGraph.filter (clr_range pr) points ) *)

(* (\* val model_of_image : Rgb24.t -> MyTimeGraph.t -> (MyModel.t * MyProp.env) *\) *)
(* let model_of_image = fun rgbimg time -> *)
(*   let (space,space_env) = space_of_image rgbimg in *)
(*   let model = MyModel.st_make space time in *)
(*   let tdom = MyModel.time_domain time in *)
(*   let pr_env = fun pr -> *)
(*     match pr with *)
(*     | MyProp.Id s -> MyProp.empty_env s *)
(*     | _ -> MyModel.st_cartesian_product (space_env pr) tdom *)
(*   in (model,pr_env) *)



(** Interfaccia **)
type screen_command = Draw of Rgb24.t | Refresh

(* val create_refresh_thread : channel -> () *)
let create_refresh_thread =
  Thread.create (fun channel ->
    while true do
      try
	Thread.delay 0.1;
	Event.sync (Event.send channel Refresh)
      with _ -> ()
    done)

(* val create_click_thread : () -> () *)
let create_click_thread =
  Thread.create (fun () ->
    while true do
      let status = Graphics.wait_next_event [Button_down;Button_up] in
      if not status.button then
	begin
	  let color = Graphics.point_color status.mouse_x status.mouse_y in
	  Printf.printf "Position: (%d,%d)\nColor: (%d,%d,%d)\n%!" status.mouse_x (size_y() - status.mouse_y) (red color) (green color) (blue color);
	  print_newline()
	end
    done)
    
(* val create_graphics_thread : channel -> () *)
let create_graphics_thread =
  Thread.create (fun channel ->
    let img = ref None in
    let size = ref (size_x (), size_y ()) in
    let redraw () =
      match !img with
	None -> ()
      | Some img ->
	clear_graph ();
	Graphic_image.draw_image (Rgb24 (Rgb24.resize None img (size_x ()) (size_y ()))) 0 0 in
    while true do
      let evt = Event.sync (Event.receive channel) in
      match evt with
	Draw newimg ->
	  img := Some newimg;
	  redraw ();
      | Refresh ->
	let newsize = (size_x (), size_y ()) in
	if !size <> newsize
	then (size := newsize; redraw ())
    done)
    
(* val draw_rgb : Rgb24.t -> unit *)
let draw_rgb =
  let channel = Event.new_channel () in
  let inited = ref false in
  let init () =
    begin
      open_graph "";
      ignore (create_click_thread ());
      ignore (create_refresh_thread channel);
      ignore (create_graphics_thread channel)
    end in
  (fun img ->
    if not (!inited) then init ();
    Event.sync (Event.send channel (Draw img)))
	

(* album, una collezione di immagini *)
type album = MyTimeGraph.point -> Rgb24.t
  
(* (\* val model_img_to_album : MyModel.st -> Rgb24.t -> album *\) *)
(* let model_img_to_album = fun model img -> *)
(*   let time = MyModel.st_time model in *)
(*   let tdom = MyTimeGraph.get_nodes time in *)
(*   fun t -> if MyTimeGraph.mem t tdom then img else failwith "Tempo non definito." *)

(* val draw_space_rgb_points : Rgb24.t -> MySpaceGraph.pointset -> Color.rgb -> Rgb24.t *)  
let draw_space_rgb_points rgbimg points color =
  let rgbimg2 = Rgb24.copy rgbimg in
  MySpaceGraph.iter (fun (x,y) ->  Rgb24.set rgbimg2 x y (over color (Rgb24.get rgbimg2 x y))) points;
  rgbimg2

(* val draw_rgb_points : album -> MyModel.st_pointset -> Color.rgb -> album *)
let draw_rgb_points = fun album stset color ->
  let space_sec = MyModel.st_space_section stset in
  fun t -> draw_space_rgb_points (album t) (space_sec t) color







(*** ultime modifiche ***)







(* dato uno spazio e un'immagine restituisce l'ambiente spaziale relativo all'immagine *)
let space_env_of_image = fun space rgbimg ->
  let points = MySpace.domain space in
  let clr_range = fun pr (x,y) ->
    match pr with
    | MyProp.RedRange (rd,ru) -> let rxy = (Rgb24.get rgbimg x y).r in rd<=rxy && rxy<=ru
    | MyProp.GreenRange (gd,gu) -> let gxy = (Rgb24.get rgbimg x y).g in gd<=gxy && gxy<=gu
    | MyProp.BlueRange (bd,bu) -> let bxy = (Rgb24.get rgbimg x y).b in bd<=bxy && bxy<=bu
    | _ -> MyProp.empty_env pr
  in
    fun pr -> MySpaceGraph.filter (clr_range pr) points

(* data un'immagine crea lo spazio corrispondente *)
let space_of_image rgbimg =
  let points =  digital_subspace (0,0) (rgbimg.Rgb24.width - 1,rgbimg.Rgb24.height - 1) in
  let neighbours (x,y) =
    set_of_list
      (List.filter
	 (fun (a,b) -> a >= 0 && a < rgbimg.Rgb24.width && b >= 0 && b < rgbimg.Rgb24.height)
	 (* chiusura square *)
	 (* [(x-1,y-1);(x-1,y);(x-1,y+1);(x,y-1);(x,y+1);(x+1,y-1);(x+1,y);(x+1,y+1)]) in *)
	 (* chiusura diamond *)
	 [(x-1,y);(x,y-1);(x,y+1);(x+1,y)]) in
  let clos = (fun p -> MySpaceGraph.fold (fun el res -> MySpaceGraph.union (neighbours el) res) p p) in
  MySpaceGraph.set_nodes points
      (MySpaceGraph.set_source neighbours
	 (MySpaceGraph.set_destination neighbours
	    (MySpaceGraph.set_closure clos (MySpaceGraph.empty))))


(* mappa da timepoint a immagine (album fatto bene) *)
module TimeImgMap = Map.Make(MyTimePoint)


(* data un nome di immagine e un tempo restituisce il modello, l'ambiente e l'album *)
let model_of_imgname_time = fun imgname time ->
  (* costruisce il modello usando una delle immagini *)
  let tdom = MyModel.time_domain time in
  let tpoint_temp = MyTime.choose tdom in
  let img_temp = load_image (imgname^"_"^(MyModel.string_of_time_point tpoint_temp)^".bmp") in
  let space = space_of_image img_temp in
  let model = MyModel.st_make space time in
  (* costruisce l'album *)
  let album_map = 
    let album_map_temp = ref(TimeImgMap.empty) in
    let _ = MyTime.iter (fun t->
		 album_map_temp:= TimeImgMap.add t (load_image (imgname^"_"^(MyModel.string_of_time_point t)^".bmp")) !(album_map_temp)
		) tdom in
    !(album_map_temp)
  in
  let album = fun t ->
    TimeImgMap.find t album_map
  in
  (* restituisce l'ambiente per valutare le proposizioni *)
  let pr_env = fun pr ->
    match pr with
    | MyProp.Id s -> MyProp.empty_env s
    | _ ->
       let stset = ref (MyModel.st_empty) in
       let smart_iter = fun t -> stset := MyModel.st_union (!stset) (MyModel.st_cartesian_product ((space_env_of_image space (album t)) pr) (MyTime.singleton t)) in
       let _ = MyTime.iter (smart_iter) tdom in
       !stset
  in (model,pr_env,album)


(* dato il file dot del tempo (come stringa), restituisce lo stesso file con il nodo clt colorato di rosso *)
let make_time_dot = fun dottmp_name dotstr clt ->
  let dotarg = String.sub dotstr 0 ((String.length dotstr)-1) in
  let oc = open_out dottmp_name in
  let counter = ref 0 in
  let str = dotarg^(string_of_int clt)^"[color=red];\n}" in
  output oc str (!counter) (String.length str);
  close_out oc

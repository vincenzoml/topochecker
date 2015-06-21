open Ccsmc
open Ccsmc.PictureLogic
open Ccsmc.DigitalPlane
open Images
open Graphics

let red color = (color / (256 * 256)) mod 256 
let green color = (color / 256) mod 256 
let blue color = color mod 256 
  
let load_image filename =
  match Bmp.load filename [] with
  | Rgb24 rgbimg -> rgbimg
  |  _ -> failwith "Only RGB24 bmp images supported at the moment."
    
let save_image img filename =
  Bmp.save filename [] (Rgb24 img)

let rec digital_subspace (xs,ys) (xe,ye) =
  let init = ref PSet.empty in
  for y = ys to ye do
    for x = xs to xe do
      init := PSet.add (x,y) (!init)
    done
  done;
  !init

let rec set_of_list l =
  match l with 
    [] -> PSet.empty
  | x::xs -> PSet.add x (set_of_list xs)

let model_of_image rgbimg =
  let points =  digital_subspace (0,0) (rgbimg.Rgb24.width - 1,rgbimg.Rgb24.height - 1) in
  let neighbours (x,y) = 
    set_of_list 
      (List.filter 
	 (fun (a,b) -> a >= 0 && a < rgbimg.Rgb24.width && b >= 0 && b < rgbimg.Rgb24.height)
	 [(x-1,y-1);(x-1,y);(x-1,y+1);(x,y-1);(x,y+1);(x+1,y-1);(x+1,y);(x+1,y+1)]) in
  { space =
      { points = points;
	pre = neighbours;
	post = neighbours;
	clos = (fun p -> PSet.fold (fun el res -> PSet.union (neighbours el) res) p p)};
    eval = fun pred -> PSet.filter (fun (x,y) -> pred (Rgb24.get rgbimg x y)) points }

let string_of_color rgb = Printf.sprintf "#%02X%02X%02X" rgb.r rgb.g rgb.b

type screen_command = Draw of Rgb24.t | Refresh

let create_refresh_thread =
  Thread.create (fun channel -> 
    while true do
      try
	Thread.delay 0.1;
	Event.sync (Event.send channel Refresh)
      with _ -> ()
    done)

let create_click_thread =
  Thread.create (fun () ->
    while true do
      let status = Graphics.wait_next_event [Button_down;Button_up] in
      if not status.button then 
	begin
	  let (x,y) = (status.mouse_x,status.mouse_y) in
	  let color = Graphics.point_color x y in
	  Printf.printf "(%d,%d) [RED = %d] & [GREEN = %d] & [BLUE = %d]\n%!" x y (red color) (green color) (blue color)
	end
    done)

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
	
let over factor c1 c2 =
  let value x y = int_of_float (((float_of_int x) *. factor) +. ((float_of_int y) *. (1.0 -. factor))) in
  { r = value c1.r c2.r;
    g = value c1.g c2.g;
    b = value c1.b c2.b; }
    
let draw_rgb_points factor rgbimg points color =
  let rgbimg2 = Rgb24.copy rgbimg in
  PSet.iter (fun (x,y) ->  Rgb24.set rgbimg2 x y (over factor color (Rgb24.get rgbimg2 x y))) points;
  rgbimg2
  

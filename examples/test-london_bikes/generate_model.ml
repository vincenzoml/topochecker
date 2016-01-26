let _ =
  let node_size = 0.03 in
  let node_color = "red" in
  let arc_color = "#0000aa20" in
  let threshold = 0.8 in (* maximum distance between neighbours in km *)
  let values = Csv.load "xyid.csv" in
  let num_nodes = List.length values in
  let vector = Array.create num_nodes (0.0,0.0,"",ref 0) in
  let id = ref 0 in
  let (bgminx,bgminy,bgmaxx,bgmaxy) = (-.8.47625,-.5.38175,7.75964,4.33756) in
  let (bgx,bgy,bgwidth,bgheight) = ((bgmaxx +. bgminx)/.2.,(bgmaxy+.bgminy)/.2.,bgmaxx-.bgminx,bgmaxy-.bgminy) in
  let output2 = open_out "london_map_transport_graphical.dot" in
  let output = open_out "london_map_transport.dot" in
  Printf.fprintf output "digraph{\n%!";
  Printf.fprintf output2 "digraph{\n%!";
  Printf.fprintf output2
    "background [image=\"london_map_transport.png\",fixedsize=true; label=\"\"; shape=rectangle; color=white; pos=\"%f,%f!\"; width=\"%f\"; height=\"%f\"; ]; \n%!"
    bgx bgy bgwidth bgheight;
  List.iter
    (function x::y::name::_ ->
      let (rx,ry) = (float_of_string x,float_of_string y) in
      vector.(!id) <- (rx,ry,name,ref 0);
      Printf.fprintf output2 "%d [ style=filled, label=\"\", color=\"%s\", fillcolor=\"%s\", fixedsize=true, height=%f, width=%f, pos = \"%f,%f!\" ];\n%!"
	!id node_color node_color node_size node_size rx ry;
      Printf.fprintf output "%d [ label=\"\", fixedsize=true, height=%f, width=%f, pos = \"%f,%f!\" ];\n%!"
	!id (3. *. node_size) (3. *. node_size) rx ry;
      (* NOTE: the name of each node is currently unused! *)
      for i = 0 to !id - 1 do
	let (x,y,_,deg) = vector.(i) in
	let (myx,myy,_,mydeg) = vector.(!id) in
	let dst = (sqrt (((myx -. x) ** 2.0) +. ((myy -. y) ** 2.0))) in
	if dst <= threshold then
	  begin
	    Printf.fprintf output
	      "%d -> %d [color=\"%s\",arrowsize=.2];\n%d -> %d [color=\"%s\",arrowsize=.2];\n%!" 
	      !id i arc_color i !id arc_color;
	    Printf.fprintf output2
	      "%d -> %d [color=\"%s\",arrowsize=.2];\n%d -> %d [color=\"%s\",arrowsize=.2];\n%!" 
	      !id i arc_color i !id arc_color;
	    deg := !deg + 1;
	    mydeg := !mydeg + 1;
	  end
      done;
      id := 1 + !id)
    values;
  Printf.fprintf output "}\n%!";
  Printf.fprintf output2 "}\n%!";
  close_out output;
  close_out output2;
  let min = ref num_nodes in
  let max = ref 0 in    
  for i = 0 to num_nodes - 1 do
    let (_,_,_,deg) = vector.(i) in
    if !deg < !min then min := !deg;
    if !deg > !max then max := !deg;
    if !deg = 0 then Printf.printf "found: %d\n%!" i
  done;
  Printf.printf "degree: min %d, max %d\n%!" !min !max
    
    

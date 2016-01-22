let _ =
  let node_size = 0.03 in
  let threshold = 0.5 in (* maximum distance between neighbours in km *)
  let values = Csv.load "xyid.csv" in
  let num_nodes = List.length values in
  let matrix = Array.make_matrix num_nodes num_nodes 0.0 in
  let vector = Array.create num_nodes (0.0,0.0,"") in
  let id = ref 0 in
  let (bgminx,bgminy,bgmaxx,bgmaxy) = (-.8.47625,-.5.38175,7.75964,4.33756) in
  let (bgx,bgy,bgwidth,bgheight) = ((bgmaxx +. bgminx)/.2.,(bgmaxy+.bgminy)/.2.,bgmaxx-.bgminx,bgmaxy-.bgminy) in
  let output = open_out Sys.argv.(1) in
  Printf.fprintf output "digraph{\n%!";
  Printf.fprintf output
    "background [image=\"london_map_transport.png\",fixedsize=true; label=\"\"; shape=rectangle; color=white; pos=\"%f,%f!\"; width=\"%f\"; height=\"%f\"; ]"
    bgx bgy bgwidth bgheight; 
  List.iter
    (function x::y::name::_ ->
      let (rx,ry) = (float_of_string x,float_of_string y) in
      vector.(!id) <- (rx,ry,name);
      Printf.fprintf output
	"%d [ style=filled; label=\"\"; color=red; fillcolor=red; fixedsize=true; height=%f; width=%f; pos = \"%f,%f!\" ];\n%!"
	!id node_size node_size rx ry;
      (* NOTE: the name of each node is currently unused! *)
      for i = 0 to !id - 1 do
	let dst = (sqrt ((rx ** 2.0) +. (ry ** 2.0))) in
	matrix.(i).(!id) <- dst;
	matrix.(!id).(i) <- dst;
      done;
      id := 1 + !id)
    values;
  Printf.fprintf output "}\n%!";
  close_out output
    

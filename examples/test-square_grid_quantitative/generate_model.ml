

let num_time = 10
let num_space = 10
let mkid i j = i + (num_space*j)
	    
let out_time fname =
  let ch = open_out fname in
  Printf.fprintf ch "digraph{\n";
  for i = 0 to num_time - 1 do
    Printf.fprintf ch "%d->%d;\n" i (i+1)
  done;
  Printf.fprintf ch "}\n";
  close_out ch
		     
let out_space fname =
  let ch = open_out fname in
  let out_node i j = Printf.fprintf ch "%d [ pos = \"%d,%d!\" ];\n" (mkid i j) i j in
  let out_arc (i1,j1) (i2,j2) =
    let (i1,i2) = (mkid i1 j1,mkid i2 j2) in
    Printf.fprintf ch "%d->%d [weight=3.0];\n%d->%d [weight=3.0];\n" i1 i2 i2 i1;
  in
  Printf.fprintf ch "digraph{\n";
  for j = 0 to num_space - 1 do
    for i = 0 to num_space - 1 do
      out_node i j;
      if i > 0 then out_arc (i,j) (i-1,j);
      if j > 0 then out_arc (i,j) (i,j-1);
    done
  done;           
  Printf.fprintf ch "}\n";
  close_out ch

let eval time x y = Printf.sprintf "a=%d" (mkid x y)
	    
let out_eval fname =
  let ch = open_out fname in
  for time = 0 to num_time - 1 do
    for y = 0 to num_space - 1 do
      for x = 0 to num_space - 1 do	
	Printf.fprintf ch "%d %d %s\n" time (mkid x y) (eval time x y)
      done
    done
  done;
  close_out ch
	    
let main =
  out_space "space.dot";
  out_time "kripke.dot";
  out_eval "eval.csv"

	    

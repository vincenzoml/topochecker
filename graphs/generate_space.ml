
let n = 10

let out_space fname =
  let ch = open_out fname in
  let mkid i j = i + (n*(j-1)) in
  let out_node i j = Printf.fprintf ch "%d [ pos = \"%d,%d!\" ]\n" (mkid i j) i j in
  let out_arc (i1,j1) (i2,j2) =
    let (i1,i2) = (mkid i1 j1,mkid i2 j2) in
    Printf.fprintf ch "%d->%d;\n%d->%d\n" i1 i2 i2 i1 in
  Printf.fprintf ch "digraph{";
  for i = 1 to n do
    for j = 1 to n do
      out_node i j;
      if i > 1 then out_arc (i,j) (i-1,j);
      if j > 1 then out_arc (i,j) (i,j-1);
    done
  done;           
  Printf.fprintf ch "}\n"

let main =
  out_space "space.dot"
		       

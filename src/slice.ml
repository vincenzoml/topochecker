type slice_kind = Bigarray.float64_elt
type slice_layout = Bigarray.c_layout
type slice = (float,slice_kind,slice_layout) Bigarray.Array2.t

let slice_kind = Bigarray.float64
let slice_layout = Bigarray.c_layout

let save_slice slice fname =
  let chan = Unix.openfile fname [Unix.O_CREAT;Unix.O_RDWR;Unix.O_TRUNC] 0o644 in
  let map = Bigarray.Array2.map_file chan
    (Bigarray.Array2.kind slice) (Bigarray.Array2.layout slice) true
    (Bigarray.Array2.dim1 slice) (Bigarray.Array2.dim2 slice) in
  Bigarray.Array2.blit slice map;
  Unix.close chan
    
let load_slice fname num_states num_points = 
  let chan = Unix.openfile fname [Unix.O_RDONLY] 0o644 in 
  let res = Bigarray.Array2.map_file chan slice_kind slice_layout false num_states num_points in
  Unix.close chan;
  res
  

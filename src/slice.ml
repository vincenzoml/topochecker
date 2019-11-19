type slice_kind = Bigarray.float64_elt
type slice_layout = Bigarray.c_layout
type slice = (float,slice_kind,slice_layout) Bigarray.Array2.t
type cslice = (float,slice_kind,slice_layout) Bigarray.Array1.t
  
let slice_kind = Bigarray.float64
let slice_layout = Bigarray.c_layout

let make_slice num_states num_points = Bigarray.Array2.create slice_kind slice_layout num_states num_points 
let make_cslice num_states = Bigarray.Array1.create slice_kind slice_layout num_states

let save_slice slice fname =
  let chan = Unix.openfile fname [Unix.O_CREAT;Unix.O_RDWR;Unix.O_TRUNC] 0o644 in
  let map' = Unix.map_file chan
    (Bigarray.Array2.kind slice) (Bigarray.Array2.layout slice) true
    [|Bigarray.Array2.dim1 slice;Bigarray.Array2.dim2 slice|] in
  let map = Bigarray.array2_of_genarray map' in
  Bigarray.Array2.blit slice map;
  Unix.close chan
    
let load_slice fname num_states num_points = 
  let chan = Unix.openfile fname [Unix.O_RDONLY] 0o644 in 
  let res' = Unix.map_file chan slice_kind slice_layout false [|num_states;num_points|] in
  let res = Bigarray.array2_of_genarray res' in
  Unix.close chan;
  res
  
    

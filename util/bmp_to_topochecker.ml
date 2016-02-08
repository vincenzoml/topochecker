open Images
    
let load_image filename =
  match Bmp.load filename [] with
  | Rgb24 rgbimg -> rgbimg
  |  _ -> failwith "Only RGB24 bmp images supported at the moment."

let cmd_parse_error () =
  failwith "Parse error in command line."
      
let out_time fname =
  let ch = open_out fname in
  Printf.fprintf ch "digraph{ 0; }\n";
  close_out ch

let mkid i j width = i + (width*j)

let mknode i j col width height spacefdes =
  let sid = mkid i j width in
  (*
    Printf.fprintf spacefdes "%d [ pos=\"%d,%d!\", style=filled, label=\"\", fixedsize=true,width=1,height=1, shape=rectangle, color=\"#%06X\" ];\n" sid i (-j) ((col.r lsl 16) lor (col.g lsl 8) lor col.b); *)
  Printf.fprintf spacefdes "%d [ pos=\"%d,%d!\", style=filled, label=\"\", fixedsize=true, width=1, height=1, shape=rectangle ];\n" sid i (-j);
  List.iter
    (fun (x,y) ->
      if  x >= 0 && x < width && y >= 0 && y < height
      then Printf.fprintf spacefdes "%d->%d [color=transparent]\n" sid (mkid x y width))
    [(i-1,j);(i+1,j);(i,j-1);(i,j+1)]

let mkeval img i j col width evalfdes =
  Printf.fprintf evalfdes "%d,%d,r=%d,g=%d,b=%d\n" 0 (mkid i j width) (col.r) (col.g) (col.b)

let out_space_eval imgfname spacefname evalfname =
  let img = load_image imgfname in
  let spacefdes = open_out spacefname in
  let evalfdes = open_out evalfname in
  try
    Printf.fprintf spacefdes "digraph {\n";
    let (width,height) = (img.Rgb24.width,img.Rgb24.height) in
	for j = 0 to height - 1 do
	  for i = 0 to width - 1 do
	    let col = Rgb24.get img i j in
	    mknode i j col width height spacefdes;
	    mkeval img i j col width evalfdes
	  done
	done;
    Printf.fprintf spacefdes "}\n";
    close_out spacefdes;
    close_out evalfdes
  with e ->
    failwith "exception while writing model files"

let out_experiment fname =
  let x = open_out fname in
  Printf.fprintf x "Kripke \"kripke.dot\" Space \"space.dot\" Eval \"eval.csv\";\nCheck \"0xFF0000\" TT;\n";
  close_out x
      
let _ = 
  out_time "kripke.dot";
  out_space_eval Sys.argv.(1) "space.dot" "eval.csv";
  out_experiment (String.concat "" [Filename.basename Sys.argv.(1); ".topochecker"])
    
    
  

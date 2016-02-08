open Images

let white = Color.color_parse "white"
    
let load_image filename =
  match Bmp.load filename [] with
  | Rgb24 rgbimg -> rgbimg
  |  _ -> failwith "Only RGB24 bmp images supported at the moment."

let cmd_parse_error () =
  failwith "Parse error in command line."

let parse_cmds args =
  let len = Array.length args - 1 in
  if len < 2 || len mod 2 != 0 then cmd_parse_error ()    
  else
    let rec parse_cmds_rec idx acc =
      if idx > len then acc
      else parse_cmds_rec (idx+2) ((args.(idx),load_image args.(idx+1))::acc)
    in
    parse_cmds_rec 1 []
      
let out_time fname =
  let ch = open_out fname in
  Printf.fprintf ch "digraph{ 0; }\n";
  close_out ch

let mkid i j width = i + (width*j)

let mknode i j width height spacefdes =
  let sid = mkid i j width in
  Printf.fprintf spacefdes "%d [ pos=\"%d,%d!\", fill=true ];\n" sid i (-j);
  List.iter
    (fun (x,y) ->
      if  x >= 0 && x < width && y >= 0 && y < height
      then Printf.fprintf spacefdes "%d->%d\n" sid (mkid x y width))
    [(i-1,j);(i+1,j);(i,j-1);(i,j+1)]

let mkeval i j prop width evalfdes =
  Printf.fprintf evalfdes "%d,%d,%s\n" 0 (mkid i j width) prop

let out_space_eval prop_image spacefname evalfname =
  let spacefdes = open_out spacefname in
  let evalfdes = open_out evalfname in
  try
    Printf.fprintf spacefdes "digraph {\n";
    let visited = Hashtbl.create 100 in
    List.iter
      (fun (prop,img) ->
	let (width,height) = (img.Rgb24.width,img.Rgb24.height) in
	for j = 0 to height - 1 do
	  for i = 0 to width - 1 do
	    if not (Hashtbl.mem visited (i,j))
	    then
	      begin
		mknode i j width height spacefdes;
		Hashtbl.add visited (i,j) ()
	      end;
	    let x = Rgb24.get img i j in
	    if (x.r,x.g,x.b) <> (255,255,255)
	    then mkeval i j prop width evalfdes
	  done
	done
      )
      prop_image;
    Printf.fprintf spacefdes "}\n";
    close_out spacefdes;
    close_out evalfdes
  with e ->
    failwith "exception while writing model files"
	    
let main args =
  let parsed = parse_cmds args in
  out_time "kripke.dot";
  out_space_eval parsed "space.dot" "eval.csv"

let _ = main Sys.argv
  

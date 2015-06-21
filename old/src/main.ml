open Ccsmc
open Ccsmc.PictureLogic
open Ccsmc.DigitalPlane
open Image
      
let split_extension filename =
  try 
    let i = String.rindex filename '.' in
    (String.sub filename 0 i,String.sub filename (i+1) (String.length filename - i - 1))
  with _ -> (filename,"");;

let _ =
  let imagename = Sys.argv.(1) in
  let rgbimg = ref (load_image imagename) in
  let model = model_of_image (!rgbimg) in
  let env = ref Env.empty in
  let (lexbufs,dst) =
    match ((try Some (open_in Sys.argv.(2)) with _ -> None), (try Some Sys.argv.(3) with _ -> None)) with
      (None,_) -> draw_rgb (!rgbimg);(ref [Lexing.from_channel stdin],None)
    | (Some in1,None) -> draw_rgb (!rgbimg); (ref [Lexing.from_channel in1;Lexing.from_channel stdin],None)
    | (Some in1,dst) -> (ref [Lexing.from_channel in1],dst) in
  let counter = ref 0 in
  let reload () =
    match dst with
      None -> draw_rgb (!rgbimg)
    | Some filename -> 	    
      let realfilename = if !counter=0 then filename else 
	  let (name,ext) = split_extension filename in
	  Printf.sprintf "%s_%d.%s" name (!counter) ext in
      save_image (!rgbimg) realfilename in
  while (!lexbufs) != [] do    
    let lexbuf = List.hd (!lexbufs) in
    try
      let syntax = Parser.main Lexer.token lexbuf in
      match syntax with
	ASK cfsyntax ->
	let cformula = cformula_of_cfsyntax (!env) cfsyntax in
	Printf.printf "%b\n%!" (ccheck model (model.space.points) cformula)
      | ASKSET (cfsyntax, points_list) ->
	 let cformula = cformula_of_cfsyntax (!env) cfsyntax in
	 Printf.printf "%b\n%!" (ccheck model (List.fold_left
						 (fun acc (x,y) -> PSet.add (x,y) acc)
						 PSet.empty
						 points_list) cformula)
      | PAINT (c, fsyntax) ->
	  let color = 
	    (match c with
	      Picture.COL (Picture.RGB (r,g,b)) -> { Color.r = r; Color.g = g; Color.b = b }
	    | Picture.COL (Picture.COLOR s) -> Color.color_parse s) in
	  let formula = formula_of_fsyntax (!env) fsyntax in
	  let points = check model formula in
	  let img2 = draw_rgb_points 1.0 (!rgbimg) points color in
	  rgbimg := img2;
	  reload ()
      | LET (ide,formalargs,fsyntax) -> 
	env := Env.add ide (fun_of_decl ide (!env) formalargs fsyntax) (!env)
      | RESET ->
	rgbimg := load_image imagename;
	counter := !counter + 1;
	reload ()
    with
      Lexer.Eof -> lexbufs := List.tl (!lexbufs)
    | exn -> 
      let msg = Printexc.to_string exn in
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
      let tok = Lexing.lexeme lexbuf in
      Printf.printf "line %d, character %d, token %s: %s\n%!" line cnum tok msg
  done

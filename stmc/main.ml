open StlLogic
open Graph
open Model
open StlConvert
open Interface
open Test

let imagefolder = Test.imagefolder

let rgbimg = Test.rgbimg
let oldalbum = ref(Test.album)
let album = ref(!oldalbum)

let dot_string = Test.dot_string
let dottmp_name = Test.dottmp_name

let time = Test.time
let (model,pr_env) = (Test.model,ref Test.pr_env)

let fsyntax_env = Test.fsyntax_env
type mutable_env = Test.mutable_env
let fs_env = Test.fs_env

let store_name =
  if Sys.argv.(3) = ""
  then "formula.fr"
  else Sys.argv.(3)

let _ =
  let _ =
    if Sys.file_exists store_name
    then 0
    else Sys.command ("touch "^store_name)
  in
  let ic = open_in store_name in
  let control = ref true in
  while !control do
    try
      let nl = input_line ic in
      let buf = Lexing.from_string nl in
      let inp = Parser.main Lexer.token buf in
      match inp with
      | Interface.LET (ide,varlist,fs) ->
	 fs_env.env <- MyLogic.bind_mvar ide fs varlist fs_env.env
      | _ -> control := false
    with
    | _ -> control := false
  done



let line_counter = ref 0
let t0 = ref 0
let s0 = ref (0,0)
let f0 = ref (MyLogic.FALSE)
let fset0 = ref (MyModel.st_empty)





(* la funzione che fa girare il programma *)
let rec reload() =
  print_newline();
  let lexbuf = Lexing.from_channel stdin in
  try
    let input = Parser.main Lexer.token lexbuf in
    match input with
     
    (* mostra le formule memorizzate *)
    | Interface.SHOW_STORE ->
      MyLogic.print_env fs_env.env;
      reload()
	
    (* mostra i possibili futuri *)
    | Interface.SHOW_FUTURE ->
      let time = MyModel.st_time model in
      let tset = MyModel.time_next (!t0) time in
      let (x,y) = xyimage_to_xyspace rgbimg (!s0) in
      let smart_print = fun t ->
	let point = MyModel.st_make_point (x,y) t in
	Printf.printf "%s --> %B\n" (MyModel.string_of_time_point t) (MyModel.st_mem point (!fset0))
      in
      Printf.printf "Space: (%d,%d)\nFormula: %s\n" (fst(!s0)) (snd(!s0)) (MyLogic.string_of_fsyntax (!f0));
      MyModel.time_iter smart_print tset;
      reload()

    (* mostra la posizione attuale nello spazio *)
    | Interface.SHOW_SPACE clr ->
      let (x,y) = xyimage_to_xyspace rgbimg (!s0) in
      let point = MyModel.st_make_point (x,y) (!t0) in
      let temp_album = draw_rgb_points (!album) (MyModel.st_add point MyModel.st_empty) (color_to_rgb clr) in
      draw_rgb (temp_album (!t0));
      Printf.printf "Space: (%d,%d)" (fst(!s0)) (snd(!s0));
      print_newline();
      reload()

    (* mostra la posizione attuale nel tempo *)
    | Interface.SHOW_TIME ->
      Printf.printf "Time: %d" (!t0);
      print_newline();
      reload()

    (* mostra la formula zero attuale *)
    | Interface.SHOW_FORMULA ->
      Printf.printf "Formula: %s" (MyLogic.string_of_fsyntax (!f0));
      print_newline();
      reload()

    (* fornisce lo stato attuale *)
    | Interface.SHOW_STATUS ->
      let (x,y) = xyimage_to_xyspace rgbimg (!s0) in
      let point = MyModel.st_make_point (x,y) (!t0) in
      Printf.printf "Space: (%d,%d)\n" (fst(!s0)) (snd(!s0));
      Printf.printf "Time: %d\n" (!t0);
      Printf.printf "Formula: %s --> %B\n" (MyLogic.string_of_fsyntax (!f0)) (MyModel.st_mem point (!fset0));
      reload()
	
    (* imposta il punto zero *)
    | Interface.SET_SPACE (x,y) ->
      s0 := (x,y);
      reload()

    (* imposta il tempo zero *)
    | Interface.SET_TIME t1 ->
      t0 := t1;
      draw_rgb ((!album) t1);
      make_time_dot (dottmp_name^".dot") dot_string t1;
      let _ =Sys.command ("dot -Tpng "^dottmp_name^".dot > "^dottmp_name^".png") in
      reload()

    (* memorizza una nuova formula *)
    | Interface.LET (ide,varlist,fs) ->
      fs_env.env <- MyLogic.bind_mvar ide fs varlist fs_env.env;
      reload()
	
    (* calcola la semantica di una formula e stampa il risultato *)
    | Interface.SEM (clr,fs) ->
       let rtime = Sys.time() in
       let fr = MyLogic.fsyntax_to_formula fs_env.env fs in
       let stset = MyLogic.sem fr model (!pr_env) in
       f0 := fs;
       fset0 := stset;
       album := draw_rgb_points (!album) stset (color_to_rgb clr);
       draw_rgb ((!album) (!t0));
       Printf.printf "Tempo impiegato: %f\n" (Sys.time() -. rtime);
       reload ()

    (* come sem ma per formule richiamate con un identificatore *)
    | Interface.SEM_IDE (clr,fride,frnamelist) ->
      let frlist = List.map (fun x -> fst(MyLogic.Env.find x fs_env.env)) frnamelist in
      let fs = MyLogic.CALL(fride,frlist) in
      let fr = MyLogic.fsyntax_to_formula fs_env.env fs in
      let stset = MyLogic.sem fr model (!pr_env) in
      f0 := fs;
      fset0 := stset;
      album := draw_rgb_points (!album) stset (color_to_rgb clr);
      draw_rgb ((!album) (!t0));
      reload ()

    (* funzione di scrittura *)
    | Interface.SAVE_STORE ->
      let oc = open_out store_name in
      let counter = ref 0 in
      let print_fr = fun x y ->
	let str = "let " ^ x ^ " = " ^ (MyLogic.string_of_fsyntax (fst y) ) ^ ";\n" in
	output oc str (!counter) (String.length str)
	
      in
      MyLogic.Env.iter print_fr fs_env.env;
      close_out oc;
      reload()
	
    (* funzione di salvataggio immagini *)
    | Interface.SAVE_IMAGE filename ->
      let name_i = fun i -> Printf.sprintf "%s%s%d.bmp" imagefolder filename i in
      let save_i = fun i -> save_image ((!album) i) (name_i i) in
      let tdom = MyModel.time_domain (MyModel.st_time model) in
      MyModel.time_iter save_i tdom;
      reload()

    (* funzione di salvataggio singola immagine *)
    | Interface.SAVE_SINGLE_IMAGE (t,filename) ->
      let name = Printf.sprintf "%s%s.bmp" imagefolder filename in
      save_image ((!album) t) name;
      reload()

    (* funzione di lettura *)
    | Interface.LOAD_STORE ->
      let ic = open_in store_name in
      let control = ref true in
      while !control do
	try
	  let nl = input_line ic in
	  let buf = Lexing.from_string nl in
	  let inp = Parser.main Lexer.token buf in
	  match inp with
	  | Interface.LET (ide,varlist,fs) ->
	    fs_env.env <- MyLogic.bind_mvar ide fs varlist fs_env.env
	  | _ -> control := false
	with
	| _ -> control := false
      done;
      reload()

    (* cancella l'immagine *)
    | Interface.RESET ->
      album := (!oldalbum);
      draw_rgb ((!album) (!t0));
      reload()
	
    (* ricarica l'immagine *)
    | Interface.REFRESH ->
      draw_rgb ((!album) (!t0));
      reload()

    (* arresta il programma *)
    | Interface.STOP_TEST ->
      ()
	
  with
  | Lexer.Eof -> ()
  | exn ->
    let msg = Printexc.to_string exn in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Printf.printf "line %d, character %d, token %s: %s\n%!" line cnum tok msg; reload()
      
let _ =
  make_time_dot (dottmp_name^".dot") dot_string (!t0);
  let _ = Sys.command ("dot -Tpng "^dottmp_name^".dot > "^dottmp_name^".png;  gnome-open "^dottmp_name^".png") in
  draw_rgb ((!album) (!t0));
  reload()

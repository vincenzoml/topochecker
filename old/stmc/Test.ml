open StlLogic
open Graph
open Model
open StlConvert
open Interface


let time =
  let ic = open_in Sys.argv.(2) in
  let lexbuf = Lexing.from_channel ic in
  ParserGraph.main LexerGraph.token lexbuf

let dottmp_name = "time_tmp"
let dot_string = 
  let ic = open_in Sys.argv.(2) in
  let fstr = ref(input_line ic) in
  let _ =
    try
      while true do
	fstr:= (!fstr)^"\n"^(input_line ic)
      done
    with
    | End_of_file -> close_in ic
  in
  !fstr

let imagefolder = "saved_images/"

let (model,pr_env,album) = model_of_imgname_time Sys.argv.(1) time

let rgbimg = album 0


let fsyntax_env = MyLogic.empty_env
type mutable_env = { mutable env : MyModel.st_pointset MyLogic.parametric_fsyntax MyLogic.Env.t }
let fs_env = { env = fsyntax_env }

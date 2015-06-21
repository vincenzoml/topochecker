open Model
       
let f args =
  try
    let (model,formula) = ExpLoader.load_experiment args.(1) in
    let res = Checker.precompute model in
    res
  with      
    Invalid_argument s ->
    Util.fail (Printf.sprintf "Usage: %s FILENAME\n" Sys.argv.(0))

let _ = f Sys.argv 
	  

let isTrue x = x = 1.0
let isFalse x = x = 0.0
let valTrue = 1.0
let valFalse = 0.0
let valUtil = 2.0
let valAnd x y = if x = 1.0 && y = 1.0 then 1.0 else 0.0
let valNot x = if x = 0.0 then 1.0 else 0.0
let ofBool x = if x then valTrue else valFalse

let debug s =
  Printf.eprintf "debug: %s\n%!" s		 
		 
let fail s =
  Printf.eprintf "%s\n%!" s;
  exit 1


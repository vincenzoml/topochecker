let isTrue x = x = 1
let isFalse x = x = 0
let valTrue = 1
let valFalse = 0
let valUtil = 2
let valAnd x y = if x = 1 && y = 1 then 1 else 0
let valNot x = if x = 0 then 1 else 0

let debug s =
  Printf.eprintf "debug: %s\n%!" s		 
		 
let fail s =
  Printf.eprintf "%s\n%!" s;
  exit 1


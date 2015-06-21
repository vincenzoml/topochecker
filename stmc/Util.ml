let rec zip l1 l2 =
  match (l1,l2) with
    ([],_) -> []
  | (_,[]) -> []
  | (x::xs,y::ys) -> (x,y)::(zip xs ys)


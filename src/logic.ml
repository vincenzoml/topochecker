type formula =
    T
  | Prop of string
  | VProp of string * string * float
  | Not of formula
  | And of formula * formula
  | Near of formula
  | Surrounded of formula * formula
  | Statcmp of string * formula * string * formula * float * float * float * int
  | Scmpima of string * string * formula * float * float * float * int
  | Asm of string * formula * float
  | Eucl of formula
  | EDT of formula
  | EDTM of formula
  | ModDijkstraDT of formula
  | Threshold of string * float * formula
  | Ex of formula
  | Af of formula
  | Eu of formula * formula
  | Ifthenelse of cformula * formula * formula
  | Maxvol of formula 
  | CC of formula
and
  cformula =
    Ctrue
  | Cand of cformula * cformula
  | Cnot of cformula
  | Cshare of formula * cformula
  | Cgroup of formula

let rec string_of_formula formula =
  Printf.sprintf "(%s)" (
    match formula with
      T -> "TT"
    | CC f -> Printf.sprintf "CC (%s)" (string_of_formula f)
    | Prop p -> Printf.sprintf "[%s]" p
    | VProp (p,op,k) -> Printf.sprintf "[%s %s %f]" p op k
    | Not f -> Printf.sprintf "! %s" (string_of_formula f)
    | And (f1,f2) -> Printf.sprintf "%s & %s" (string_of_formula f1) (string_of_formula f2)
    | Near f -> Printf.sprintf "N %s" (string_of_formula f)
    | Surrounded (f1,f2) -> Printf.sprintf "%s S %s" (string_of_formula f1) (string_of_formula f2)
    | Statcmp (s1,f1,s2,f,k1,k3,k4,i) -> Printf.sprintf "SCMP(%s,%s,%s,%s,%f,%f,%f,%d)" s1 (string_of_formula f1) s2 (string_of_formula f) k1 k3 k4 i
    | Scmpima (s1,s2,f,k1,k3,k4,i) -> Printf.sprintf "SCMPIMA(%s,%s,%s,%f,%f,%f,%d)" s1 s2 (string_of_formula f) k1 k3 k4 i
    | Asm (s1,f,k) -> Printf.sprintf "ASM(%s,%s,%f)" s1 (string_of_formula f) k
    | Eucl f -> Printf.sprintf "Eucl %s" (string_of_formula f)
    | EDT f -> Printf.sprintf "EDT %s" (string_of_formula f)
    | EDTM f -> Printf.sprintf "EDTM %s" (string_of_formula f)
    | ModDijkstraDT f -> Printf.sprintf "ModDijkstraDT %s" (string_of_formula f)
    | Threshold (s,k,f) -> Printf.sprintf "Thr(%s,%f) %s" s k (string_of_formula f)
    | Ex f -> Printf.sprintf "EX %s" (string_of_formula f)
    | Af f -> Printf.sprintf "AF %s" (string_of_formula f)
    | Eu (f1,f2) ->
       Printf.sprintf "%s EU %s" (string_of_formula f1) (string_of_formula f2)
    | Ifthenelse (cf,f1,f2) ->
       Printf.sprintf "if %s then %s else %s fi" (string_of_cformula cf) (string_of_formula f1) (string_of_formula f2)
    | Maxvol f ->
       Printf.sprintf "maxvol %s" (string_of_formula f))
and
    string_of_cformula cformula =
  Printf.sprintf "(%s)" (
    match cformula with
      Ctrue -> "TT"
    | Cand (cf1,cf2) -> Printf.sprintf "%s & %s" (string_of_cformula cf1) (string_of_cformula cf2)
    | Cnot cf -> Printf.sprintf "! %s" (string_of_cformula cf)
    | Cshare (f,cf) -> Printf.sprintf "%s -< %s" (string_of_formula f) (string_of_cformula cf)
    | Cgroup f -> Printf.sprintf "Gr %s" (string_of_formula f)
  )
     
type qformula =
  | QFloat of float
  | QOp of (float -> float -> bool) * qformula * qformula
  | QCount of formula

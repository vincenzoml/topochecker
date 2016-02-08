type formula =
    T
  | Prop of string
  | VProp of (string * string * float)
  | Not of formula
  | And of formula * formula
  | Near of formula
  | Surrounded of formula * formula 
  | Ex of formula
  | Af of formula
  | Eu of formula * formula     
      
type qformula =
  | QFloat of float
  | QOp of (float -> float -> bool) * qformula * qformula
  | QCount of formula
      

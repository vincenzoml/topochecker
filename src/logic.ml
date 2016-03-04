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
  | Coll of cformula
 and cformula =
     CT
   | CNot of cformula
   | CAnd of cformula * cformula
   | CGroup of formula
   | CShare of formula * cformula	  
		      
type qformula =
  | QFloat of float
  | QOp of (float -> float -> bool) * qformula * qformula
  | QCount of formula
      

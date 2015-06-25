type formula =
    T
  | Prop of string
  | Not of formula
  | And of formula * formula
  | Near of formula
  | Surrounded of formula * formula
  | Ex of formula
  | Af of formula
  | Eu of formula * formula     

(*		      
 and cformula =
   CT
   | CNot of cformula
   | CAnd of cformula * cformula
   | CGroup of formula
   | CShare of formula * cformula
 *)		 

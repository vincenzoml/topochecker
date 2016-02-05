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

type qatom = Qint of int | Qformula of formula
      
type qformula =
    QT
  | QNot of qformula
  | QAnd of qformula * qformula
  | QOp of (int -> int -> bool) * qatom * qatom
  | QCount of qatom
      

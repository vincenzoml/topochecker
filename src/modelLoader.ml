open Bigarray
    
type command =
    Check of string * Logic.formula
  | Output of string * (int list option)
  | Ask of string * (string list) * Logic.qformula

let load_ask_query env string =
  let lexbuf = Lexing.from_string string in
  let (id,points,qfsyn) = TcParser.ask TcLexer.token lexbuf in  
  (id,points,Syntax.qformula_of_qfsyn env qfsyn)
    
let model_loaders =
(*  [|DotParser.load_dot_model; NiftiParser.load_nifti_model; IlcParser.load_ilc_model|] *)
  [|DotParser.load_dot_model;NiftiParser.load_nifti_model|]
    
let load_model dir kripkef spacef evalf =  
  let i = ref 0 in
  let res = ref None in
  while !i < Array.length model_loaders && !res = None do
    (match model_loaders.(!i) dir kripkef spacef evalf with
      None -> ()
    | Some r -> res := Some r);
    i := !i + 1;
  done;
  match !res with
    None -> Util.fail (Printf.sprintf "model kripke=%s space=%s eval=%s not loadable" kripkef spacef evalf)
  | Some model -> Model.completeDeadlocks model
    
let load_experiment =
  fun path ->
  let (dir,file) = (Filename.dirname path,Filename.basename path) in
  let desc = open_in (Util.mkfname dir file) in
  let lexbuf = Lexing.from_channel desc in
  try
    let (Syntax.MODEL (kripkef,spacef,evalf),dseq,commands) = TcParser.main TcLexer.token lexbuf in
    let model = load_model dir kripkef spacef evalf in
    let env = Syntax.env_of_dseq dseq in
    let commands = List.map (function
      | Syntax.CHECK (color,fsyn) -> Check (color,Syntax.formula_of_fsyn env fsyn)
      | Syntax.ASK (ide,points,qfsyn) -> Ask (ide,points,Syntax.qformula_of_qfsyn env qfsyn)
      | Syntax.OUTPUT (s,None) -> Output (s,None)
      | Syntax.OUTPUT (s,Some states) -> Output (s,Some (List.map model.Model.idkripke states))) commands in    
    close_in desc;
    (model,env,commands)
  with exn ->
    close_in desc;
    let msg = Printexc.to_string exn in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Util.fail (Printf.sprintf "filename: %s, line %d, character %d, token %s: %s\n%!" file line cnum tok msg)
      

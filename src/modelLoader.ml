open Bigarray
    
type command =
    Check of string * Logic.formula
  | Output of string * (int list option)
  | Ask of string * (string list) * Logic.qformula

let load_ask_query env string =
  let lexbuf = Lexing.from_string string in
  let (id,points,qfsyn) = TcParser.ask TcLexer.token lexbuf in  
  (id,points,Syntax.qformula_of_qfsyn env qfsyn)

let uri_model_loaders = (* todo add uri parser for dot models! *)
  [("med",NiftiParser.load_nifti_model);
   ("bmpdot",BmpDotParser.load_bmpdot_model);
   (*("img",ImgParser.load_img_model)*)]
    
let load_model dir model =
  match model with
    Syntax.URI uri ->
      let [protocol;data] = Str.split (Str.regexp ":") uri in
      let components : string list = Str.split (Str.regexp ",") data in
      let bindings = List.map (fun x -> match Str.bounded_split (Str.regexp "=") x 2 with [s] -> ("",s) | [s1;s2] -> (s1,s2)) components in
      let loader = List.assoc protocol uri_model_loaders in
      Model.completeDeadlocks (loader bindings)
  | Syntax.MODEL (kripkef,spacef,evalf) -> (* deprecated *)
      Model.completeDeadlocks (DotParser.load_dot_model dir kripkef spacef evalf)
       
let load_experiment =
  fun path ->
  let (dir,file) = (Filename.dirname path,Filename.basename path) in
  let desc = open_in (TcUtil.mkfname dir file) in
  let lexbuf = Lexing.from_channel desc in
  try
    let (msyn,dseq,commands) = TcParser.main TcLexer.token lexbuf in
    let model = load_model dir msyn in
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
    TcUtil.fail (Printf.sprintf "%s:%d:%d: token %s: %s\n%!" file line cnum tok msg)
      

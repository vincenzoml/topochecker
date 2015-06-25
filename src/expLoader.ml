let mkfname dir file =
  if Filename.is_relative file then dir ^ Filename.dir_sep ^ file else file							      
let load_experiment path =  
  let (dir,file) = (Filename.dirname path,Filename.basename path) in  
  let lexbuf = Lexing.from_channel (open_in (mkfname dir file)) in
  try
    let (syntax,dseq,command) = ExpParser.main ExpLexer.token lexbuf in
    match (syntax,dseq,command) with
      (Syntax.MODEL (kripkef,spacef,evalf),dseq,Syntax.CHECK fsyn) ->
      let model = DotParser.load_model (mkfname dir kripkef) (mkfname dir spacef) (mkfname dir evalf) in
      let env = Syntax.env_of_dseq dseq in
      let formula = Syntax.formula_of_fsyn env fsyn in
      (model,formula)
  with exn -> 
    let msg = Printexc.to_string exn in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Util.fail (Printf.sprintf "filename: %s, line %d, character %d, token %s: %s\n%!" file line cnum tok msg)
	      

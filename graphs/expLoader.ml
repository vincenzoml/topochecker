let load_experiment filename =
  let lexbuf = Lexing.from_channel (open_in filename) in
  try
    let (syntax,dseq,command) = ExpParser.main ExpLexer.token lexbuf in
    match (syntax,dseq,command) with
      (Syntax.MODEL (kripkef,spacef,evalf),dseq,Syntax.CHECK fsyn) ->
      let model = DotParser.load_model kripkef spacef evalf in
      let env = Syntax.env_of_dseq dseq in
      let formula = Syntax.formula_of_fsyn env fsyn in
      (model,formula)
    | _ -> Util.fail "collective formulas are not supported"
  with exn -> 
    let msg = Printexc.to_string exn in
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    Util.fail (Printf.sprintf "filename: %s, line %d, character %d, token %s: %s\n%!" filename line cnum tok msg)
	      

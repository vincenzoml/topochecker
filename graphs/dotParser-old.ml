open Graph.Dot_ast
			 
module ParserSig =
  struct
    let counter = ref 0
    let props = ref []
    let reset () = counter := 0; props := []
    let node ((id,_):Graph.Dot_ast.node_id) (attr_list : Graph.Dot_ast.attr list) =
      let x = !counter in
      counter := !counter + 1;
      props := (x,
		(List.concat
		   (List.map
		      (List.map
			 (fun (id,idopt) ->
			  (match id with
			     String s -> s
			   | Ident i -> i
			   | _ -> Util.fail "only string and identifiers are supported for dot files")))
		      attr_list)))::!props;
      x
    let edge _ = ()
  end

module Parser = Graph.Dot.Parse(Graph.Builder.I(Model.Space))(ParserSig)
			       
let model_of_dotfile : string -> string Model.model =
  fun filename ->
  ParserSig.reset (); (* TODO PARALLEL mutual exclusion *)  
  let parsed = Parser.parse filename in
  let propArray = Array.create !ParserSig.counter [] in
  List.iter (fun (i,v) -> Array.set propArray i v) !ParserSig.props;		  
  { Model.space = parsed;
    Model.eval = fun prop state point -> List.mem prop propArray.(id); } (* TODO EFFICIENCY avoid using list membership here *)
      

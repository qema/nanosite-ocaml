open Template
  
let run () =
  assert (eval "let x = 3 in let y = 5 + x in y" = 8);
  
  let context = [("testvar", String "h\\ni")] in
  let tmpl = of_string "{%\"{{}}\"^(String.make 3 'a');%}te\nxt{{\"expr{%%}\"}}" in
  assert (tmpl =
      [Code "\"{{}}\"^(String.make 3 'a');"; Text "te\nxt"; Expr "\"expr{%%}\""]);
  assert (fill tmpl context = "te\nxtexpr{%%}");
  assert (get_context_rep (List [String "hi"; String "bye"])
	  = "[\"hi\";\"bye\"]");
  assert (get_context_rep (Dict [(String "hi", Float 3.14);
					(Int 3, Bool true)])
		 = "[(\"hi\",3.14);(3,true)]")

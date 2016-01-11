open Template
  
let run () =
  assert (eval "let x = 3 in let y = 5 + x in y" = 8);
  assert (escape "<br>" = "&lt;br&gt;");
  let context = [("testvar", String "hi")] in
  let tmpl =
    of_string "text\nABC{{testvar^\"\"}}{{{<noescape>}}}{{String.make 3 's'}}<text>"
  in
  assert (tmpl =
      [Text "text\nABC"; Code "testvar^\"\""; Noescape "<noescape>";
    Code "String.make 3 's'"; Text "<text>"]);
  assert (fill tmpl context = "text\nABChi<noescape>sss&lt;text&gt;");
  assert (get_context_rep (List [String "hi"; String "bye"])
	  = "[\"hi\";\"bye\"]");
  assert (get_context_rep (Dict [(String "hi", Float 3.14);
					(Int 3, Bool true)])
		 = "[(\"hi\",3.14);(3,true)]")

open Template
  
let run () =
  assert (eval "let x = 3 in let y = 5 + x in y" = 8);
  assert (escape "<br>" = "&lt;br&gt;");
  assert (of_string "text{{code{{{noescape{{}}noescape}}}more_code}}text" =
      [Text "text"; Code "code"; Noescape "noescape";
       Noescape "noescape"; Code "more_code"; Text "text"])

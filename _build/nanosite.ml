(* nanosite static site generator *)
(* dependencies: batteries, omd *)
(* by Andrew Wang *)

let pages_dir_name = "pages"
let posts_dir_name = "posts"
let templates_dir_name = "templates"
let templates = ["main"; "front"; "post"; "page"]

let _ = Template.of_string "1{{2}}3{{{4}}}5"

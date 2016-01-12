(* nanosite static site generator *)
(* dependencies: omd *)
(* by Andrew Wang *)

let pages_dir_name = "pages"
let posts_dir_name = "posts"
let templates_dir_name = "templates"
let templates = ["main"; "front"; "post"; "page"]

(* Get meta attribs out of Markdown string [s]. *)
(* Returns: (meta, md) tuple where [meta] is an association list of *)
(*   attrib names to values (all strings), and [md] is the non-meta section. *)
(* These are name: value pairs put at the beginning of the file, separated *)
(*   by newlines. A blank line separates the meta section from the main one *)
(*   (if it exists). *)
let split_meta s =
  (* first check if it has a meta section by seeing it it has a valid ID *)
  (*   preceding a colon. valid means it's a valid OCaml identifier *)
  let colon_split = Str.bounded_split (Str.regexp ":") s 2 in
  let valid_id =
    Str.string_match (Str.regexp "^[_A-Z][_a-zA-Z0-9']*$")
      (List.hd colon_split) 0 in
  if List.length colon_split = 1 || not valid_id then ([], s)
  else (
    let sections = Str.split (Str.regexp "\n\n") s in
    let meta_lines = Str.split (Str.regexp "\n") (List.hd sections) in
    let meta = List.map (fun line ->
      let split = Str.bounded_split (Str.regexp ":[ \t]*") line 2 in
      (List.hd split, List.nth split 1)) meta_lines in
    let md = if List.length sections = 1 then "" else List.nth sections 1 in
    (meta, md)
  )
  
(* [ctx] is a reference to a context, which is
 *   an association list of string names to context_entries *)
let proc_posts ctx =
  let posts_dir = Sys.readdir posts_dir_name in
  Array.iter (fun filename ->
    (* only process Markdown files *)
    let path = Filename.concat posts_dir_name filename in
    if not (Sys.is_directory path) &&
      Filename.check_suffix path "md" then (
	let html =
	  Batteries.open_in path |> Batteries.IO.read_all
	  |> Omd.of_string |> Omd.to_html in
        print_endline html
    )
  ) posts_dir

let build_site () =
  proc_posts (ref [])
  
let _ = build_site ()    

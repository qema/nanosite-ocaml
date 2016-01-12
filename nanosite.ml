(* nanosite static site generator *)
(* dependencies: omd *)
(* by Andrew Wang *)

let pages_dir_name = "pages"
let posts_dir_name = "posts"
let templates_dir_name = "templates"
let templates = ["main"; "front"; "post"; "page"]

(* Get meta attribs out of Markdown string [s]. *)
(* Returns: (meta, md) tuple where [meta] is an association list of *)
(*   (lowercased) attrib names to values (all strings), and [md] is the *)
(*   non-meta section. *)
(* These are name: value pairs put at the beginning of the file, separated *)
(*   by newlines. A blank line separates the meta section from the main one *)
(*   (if it exists). *)
let split_meta s =
  (* first check if it has a meta section by seeing it it has a valid ID *)
  (*   preceding a colon. valid means it starts with an underscore or letter, *)
  (*   and the rest of the characters are _, letters or numbers.  *)
  let colon_split = Str.bounded_split (Str.regexp ":") s 2 in
  let poss_id = List.hd colon_split in
  let valid_id =
    (Str.string_match (Str.regexp "^[_a-zA-Z][_a-zA-Z0-9]*$")
      poss_id 0) && not (String.contains poss_id '\n') in
  if List.length colon_split = 1 || not valid_id then (
    ([], s)
  ) else (
    let sections = Str.split (Str.regexp "\n\n") s in
    let meta_lines = Str.split (Str.regexp "\n") (List.hd sections) in
    let meta = List.map (fun line ->
      let split = Str.bounded_split (Str.regexp ":[ \t]*") line 2 in
      (String.lowercase (List.hd split), List.nth split 1)) meta_lines in
    let md = if List.length sections = 1 then "" else List.nth sections 1 in
    (meta, md)
  )

(* load Markdown file from [path] and return (meta, html) tuple *)
(*   with the metadata dict and HTML-converted contents of the file *)
let import_md_file path =
  let s = Batteries.open_in path |> Batteries.IO.read_all in
  let (meta, md) = split_meta s in
  let html = md |> Omd.of_string |> Omd.to_html in
  (meta, html)

(* import meta.md and return a context *)    
let import_meta path =
  let (meta, _) = import_md_file path in
  List.map (fun (k, v) -> (k, Template.String v)) meta
  
(* [ctx] is a context, which is
 *   an association list of string names to context_entries.
 *   Returns an updated context with post contents. *)
let proc_dir dir_name ctx =
  let rec proc_dir' dir_name =
    let dir = Sys.readdir dir_name in
    let subdirs = ref [] in
    let posts = ref [] in
    (Array.iter (fun filename ->
      (* only process Markdown files *)
      let path = Filename.concat dir_name filename in
      if Sys.is_directory path then (
	(* get contents of "root" dir of recursive traversal *)
	let subdir_contents =
	  List.assoc (Template.String "") (proc_dir' path) in
	subdirs := (Template.String filename, subdir_contents)::!subdirs
      ) else if Filename.check_suffix path "md" then (
	  let (meta, html) = import_md_file path in
	  let meta_with_html = ("html", html)::meta in
	  let meta_dict = Template.make_dict meta_with_html in
	  posts := (Template.String filename, meta_dict)::!posts
      )
     ) dir);
    [(Template.String "", Template.Dict !posts)] @ !subdirs in
  (dir_name, Template.Dict (proc_dir' dir_name))::ctx

let proc_posts ctx =
  let res = proc_dir posts_dir_name [] in
  let dict = List.assoc posts_dir_name res in
  match dict with
  | Template.Dict d ->
     (posts_dir_name, List.assoc (Template.String "") d)::ctx
  | _ -> failwith "Error processing posts"
       
let proc_pages = proc_dir pages_dir_name
  
let build_site () =
  (* get initial context from meta.md *)
  let meta = import_meta "meta.md" in
  
  let ctx = meta |> proc_posts |> proc_pages in
  (List.iter (fun (name, value) ->
    Printf.printf "%s -> %s\n\n" name (Template.context_rep value)) ctx)
(*Template.fill [] ctx*)
  
let _ = build_site ()    

 

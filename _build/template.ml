exception EvalError

let _ = Toploop.initialize_toplevel_env()
  
(* adapted from: https://thelackthereof.org/projects/ocaml/eval/eval.ml *)
let eval code =
  let code_wrapped = "let __eval_res = " ^ code ^ ";;" in
  (try
    let lb = (Lexing.from_string code_wrapped) in
    let phr = !Toploop.parse_toplevel_phrase lb in
    ignore (Toploop.execute_phrase false Format.std_formatter phr)
   with _ -> raise EvalError);
  Obj.obj (Toploop.getvalue "__eval_res")

type template_line = Text of string
		     | Code of string
		     | Noescape of string 
type t = template_line list

(* TODO *)
let escape s = 
  s |> Str.global_replace (Str.regexp "<") "&lt;"
	 |> Str.global_replace (Str.regexp ">") "&gt;"
  
(* compile a template from a string [s] *)    
let of_string s =
  let tokens = Str.full_split (Str.regexp "{{{\\|}}}\\|{{\\|}}") s in
  let stream = Stream.of_list tokens in
  let tmpl = ref [] in
  let rec proc stream cur_type prev_type =
    if Stream.count stream = 0 then () else
    match (Stream.next stream, cur_type) with
    | (Str.Text s, Text _) -> (
      tmpl := !tmpl @ [Text (escape s)];
      proc stream cur_type prev_type )
    | (Str.Text s, Code _) -> (
      tmpl := !tmpl @ [Code (escape s)];
      proc stream cur_type prev_type )
    | (Str.Text s, Noescape _) -> (
      tmpl := !tmpl @ [Noescape s];
      proc stream cur_type prev_type )
    | (Str.Delim "{{", _) -> proc stream (Code "") cur_type
    | (Str.Delim "{{{", _) -> proc stream (Noescape "") cur_type
    | (Str.Delim "}}", _) | (Str.Delim "}}}", _) ->
       proc stream prev_type cur_type in
  proc stream (Text "") (Text "");
  !tmpl

(* fill out a template with the given context *)
(* [tmpl] is of type t, [context] *)
(*let fill tmpl context =*)
  

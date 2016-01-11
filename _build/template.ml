(* adapted from: https://thelackthereof.org/projects/ocaml/eval/eval.ml *)
let eval code =
  Toploop.initialize_toplevel_env ();
  let code_wrapped = "let __eval_res = " ^ code ^ ";;" in
  try (
    let lb = (Lexing.from_string code_wrapped) in
    let phr = !Toploop.parse_toplevel_phrase lb in
    ignore (Toploop.execute_phrase false Format.std_formatter phr);
    Obj.obj (Toploop.getvalue "__eval_res")
  ) with s -> raise s

type template_line = Text of string
		     | Code of string
		     | Noescape of string 
type t = template_line list

let escape s = 
  s |> Str.global_replace (Str.regexp "<") "&lt;"
    |> Str.global_replace (Str.regexp ">") "&gt;"
	
(* compile a template from a string [s] *)    
let of_string s =
  let tokens = Str.full_split (Str.regexp "{{{\\|}}}\\|{{\\|}}") s in
  let stream = Stream.of_list tokens in
  let tmpl = ref [] in
  
  let is_stream_empty stream = match Stream.peek stream with
    | Some _ -> false | None -> true in
  let rec proc stream cur_type =
    if is_stream_empty stream then () else
    match (Stream.next stream, cur_type) with
    | (Str.Text s, Text _) -> (
      tmpl := !tmpl @ [Text s];
      proc stream cur_type )
    | (Str.Text s, Code _) -> (
      tmpl := !tmpl @ [Code s];
      proc stream cur_type )
    | (Str.Text s, Noescape _) -> (
      tmpl := !tmpl @ [Noescape s];
      proc stream cur_type )
    | (Str.Delim "{{", _) -> (proc stream (Code ""); proc stream cur_type)
    | (Str.Delim "{{{", _) -> (proc stream (Noescape ""); proc stream cur_type)
    | (Str.Delim "}}", _) | (Str.Delim "}}}", _) -> ()
    | (_, _) -> failwith "compile template: invalid token" in
  proc stream (Text "");
  !tmpl

type context_entry = String of string | Int of int
    | Float of float | Bool of bool
let get_context_rep = function
  | String n -> "\"" ^ n ^ "\""
  | Int n -> string_of_int n
  | Float n -> string_of_float n
  | Bool n -> string_of_bool n
     
(* fill out a template with the given context *)
(* [tmpl] is of type t, [context] is an assoc list of strings to Obj.t's *)
(*   which maps variable names to the objects bound to them *)
let fill tmpl context =
  (* replace single backlash with double backlash; escape quotes, newlines *)
  let sanitized s literal = if literal then s else
      s |> Str.global_replace (Str.regexp "\\\\") "\\\\"
        |> Str.global_replace (Str.regexp "\"") "\\\""
        |> Str.global_replace (Str.regexp "\n") "\\n" in
  let add_emitter literal s =
    "__s := !__s ^ " ^ (if literal then "(" else "\"") ^
      (sanitized s literal) ^ (if literal then ");" else "\";") in
  
  let strings = List.map
    (function
    | Text s -> s |> escape |> add_emitter false
    | Code s -> s |> add_emitter true
    | Noescape s -> s |> add_emitter false) tmpl in

  let context_binds = List.map
    (fun (name, value) ->
      "let "^name^" = "^(sanitized (get_context_rep value) true)^" in\n")
    context in
  let s = List.fold_left (fun a e -> a ^ "\n" ^ e)
    "let __s = ref \"\" in" (context_binds @ strings) in
  let code = s ^ "!__s" in
  
  print_endline code;
  eval code

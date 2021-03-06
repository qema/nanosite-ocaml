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

(* replace single backlash with double backlash; escape quotes, newlines *)
let sanitized s =
  s |> Str.global_replace (Str.regexp "\\\\") "\\\\"
  |> Str.global_replace (Str.regexp "\"") "\\\""
  |> Str.global_replace (Str.regexp "\n") "\\n"
      
type template_line = Text of string
		     | Code of string
		     | Expr of string
     
type t = template_line list
	
(* compile a template from a string [s] *)    
let of_string s =
  let tokens = Str.full_split (Str.regexp "{%\\|%}\\|{{\\|}}") s in
  let stream = Stream.of_list tokens in
  let tmpl = ref [] in (* stores the template backwards (last line first) *)
  
  let is_stream_empty stream = match Stream.peek stream with
    | Some _ -> false | None -> true in
  let append tmpl cur =
    if List.length !tmpl = 0 then tmpl := [cur]
    else
      let rest = List.tl !tmpl in
      match (List.hd !tmpl, cur) with
      | (Text a, Text b) -> tmpl := Text (a ^ b)::rest
      | (Code a, Code b) -> tmpl := Code (a ^ b)::rest
      | (Expr a, Expr b) -> tmpl := Expr (a ^ b)::rest
      | (_, n) -> tmpl := n::!tmpl in
  let rec proc stream cur_type =
    if is_stream_empty stream then ()
    else (
      let next_type = ref cur_type in
      (match (Stream.next stream, cur_type) with
      | (Str.Delim "{%", Text _) -> next_type := Code ""
      | (Str.Delim "{{", Text _) -> next_type := Expr ""
      | (Str.Delim "}}", Expr _) | (Str.Delim "%}", Code _) ->
	 next_type := Text ""
      | (Str.Text s, Text _) | (Str.Delim s, Text _) -> append tmpl (Text s)
      | (Str.Text s, Code _) | (Str.Delim s, Code _) -> append tmpl (Code s)
      | (Str.Text s, Expr _) | (Str.Delim s, Expr _) -> append tmpl (Expr s));
      proc stream !next_type
    ) in
  proc stream (Text "");
  List.rev !tmpl

type context_entry = String of string | Int of int | Float of float
		     | Bool of bool | List of context_entry list
		     | Dict of (context_entry * context_entry) list
			 
let rec context_rep = function
  | String n -> "\"" ^ (sanitized n) ^ "\""
  | Int n -> string_of_int n
  | Float n -> string_of_float n
  | Bool n -> string_of_bool n
  | List n ->
     if List.length n = 0 then "[]" else
     ("[" ^ ((List.fold_left
	       (fun acc elt -> acc ^ ";" ^ (context_rep elt)))
	       (context_rep (List.hd n))
	       (List.tl n)) ^ "]")
  | Dict n ->
     let tupleize (k,v) = "(" ^ (context_rep k) ^ ","
		  ^ (context_rep v) ^ ")" in
     if List.length n = 0 then "[]" else
     ("[" ^ ((List.fold_left
	       (fun acc elt -> acc ^ ";" ^ (tupleize elt)))
	       (tupleize (List.hd n))
	       (List.tl n)) ^ "]")

(* make context_entry dict out of association list of strings *)
let make_dict lst =
  Dict (List.map (fun (k,v) -> (String k, String v)) lst)
		   
(* fill out a template with the given context *)
(* [tmpl] is of type t and is the template to fill out. *)
(* [context] is an assoc list of strings to context_entries *)
(*   which maps variable names to the objects bound to them *)
let fill tmpl context =
  let add_emitter literal s =
    "__s := !__s ^ " ^
      (if literal then "\"" else "(") ^
      (if literal then sanitized s else s) ^
      (if literal then "\";\n" else ");\n") in
  
  let strings = List.map
    (function
    | Text s -> s |> add_emitter true
    | Code s -> s
    | Expr s -> s |> add_emitter false) tmpl in

  let context_binds = List.map
    (fun (name, value) ->
      "let "^name^" = "^(context_rep value)^" in\n")
    context in
  let s = List.fold_left (fun a e -> a ^ e)
    "let __s = ref \"\" in\n" (context_binds @ strings) in
  let code = s ^ "!__s" in
  
  print_endline code;
    print_endline (eval code);
  eval code
     

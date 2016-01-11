(* adapted from: https://thelackthereof.org/projects/ocaml/eval/eval.ml *)

exception EvalError

let _ = Toploop.initialize_toplevel_env()

let eval code =
  let code_wrapped = "let __eval_res = " ^ code ^ ";;" in
  (try
    let lb = (Lexing.from_string code_wrapped) in
    let phr = !Toploop.parse_toplevel_phrase lb in
    ignore (Toploop.execute_phrase false Format.std_formatter phr)
   with _ -> raise EvalError);
  Obj.obj (Toploop.getvalue "__eval_res")

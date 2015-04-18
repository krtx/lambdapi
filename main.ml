open Core.Std
(* open Syntax *)
(* open Lexer *)
(* open Lexing *)

(*
let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
  try Parser.term_inf Lexer.read lexbuf with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  | Parser.Error ->
    fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)
*)

let _ =
  let term = Parser.prog Lexer.read (Lexing.from_channel stdin) in
  match term with
  | Some term ->
    let term = Syntax.conv_inf [] term in
    let typ = Typing.type_inf 0 [] term in
    begin
      match typ with
      | Result.Ok t -> Printer.pp_term_chk Format.std_formatter (Quote.quote_0 t)
      | Result.Error s -> print_endline s
    end
  | None -> ()

(*
let _ =
  (*  (λ α x → x) :: (∀ x :: *, ∀ y :: x, x)  *)
  let typ = Pi (Inf Star, Inf (Pi (Inf (Bound 0), Inf (Bound 1)))) in
  let id = Ann (Lam (Lam (Inf (Bound 0))), Inf typ) in
  let ctx = [(Global "Bool", VStar); (Global "False", vfree (Global "Bool"))] in
  ignore (Typing.type_inf 0 [] id);
  ignore (Typing.type_inf 0 ctx (App (App (id, Inf (Free (Global "Bool"))),
                                      Inf (Free (Global "False")))))
*)

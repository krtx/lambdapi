open Core.Std
open Syntax

type env = value list

let rec eval_inf (t : term_inf) (d : env) : value =
  match t with
  | Ann   (e, _)      -> eval_chk e d
  | Star              -> VStar
  | Pi    (tau, tau') -> VPi (eval_chk tau d, fun x -> eval_chk tau' (x :: d))
  | Bound i           -> List.nth_exn d i
  | Free  x           -> vfree x
  | App   (e, e')     -> vapp (eval_inf e d) (eval_chk e' d)

and vapp (u : value) (v : value) : value =
  match u with
  | VLam     f -> f v
  | VNeutral n -> VNeutral (NApp (n, v))
  | VStar      -> failwith "illegal application"
  | VPi (_, _) -> assert false

and eval_chk (t : term_chk) (d : env) : value =
  match t with
  | Inf i -> eval_inf i d
  | Lam e -> VLam (fun x -> eval_chk e (x :: d))

open Core.Std
open Syntax

let rec quote_0 (v : value) : term_chk = quote 0 v

and quote (i : int) (v : value) : term_chk =
  match v with
  | VLam     f      -> Lam (quote (i + 1) (f (vfree (Quote i))))
  | VNeutral n      -> Inf (neutral_quote i n)
  | VStar           -> Inf Star
  | VPi      (v, f) -> Inf (Pi (quote i v, quote (i + 1) (f (vfree (Quote i)))))

and neutral_quote (i : int) (n : neutral) : term_inf =
  match n with
  | NFree x      -> boundfree i x
  | NApp  (n, v) -> App (neutral_quote i n, quote i v)

and boundfree (i : int) (n : name) : term_inf =
  match n with
  | Quote k -> Bound (i - k - 1)
  | x       -> Free x

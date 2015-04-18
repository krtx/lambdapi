open Core.Std

(** AST used in parsing *)
module Parsing = struct
  type id = string

  type term =
    | Ann  of term * term
    | Star
    | Pi   of id * term * term
    | Var  of id
    | App  of term * term
    | Lam  of id * term
end

type term_inf =
  | Ann   of term_chk * term_chk
  | Star
  | Pi    of term_chk * term_chk
  | Bound of int
  | Free  of name
  | App   of term_inf * term_chk

and term_chk =
  | Inf of term_inf
  | Lam of term_chk                      (* de Bruijn indices *)

and name =
  | Global of string
  | Local  of int
  | Quote  of int

type value =
  | VLam     of (value -> value)         (* HOAS *)
  | VStar
  | VPi      of value * (value -> value) (* HOAS *)
  | VNeutral of neutral

and neutral =
  | NFree of name
  | NApp  of neutral * value

let vfree (n : name) : value = VNeutral (NFree n)

module P = Parsing

let find_n x lst =
  let rec loc cnt = function
    | [] -> None
    | e :: rest -> if x = e then Some cnt else loc (cnt + 1) rest
  in loc 0 lst

let rec conv_inf (env : string list) = function
  | P.Ann  (t, t')    -> Ann (conv_chk env t, conv_chk env t')
  | P.Star            -> Star
  | P.Pi   (x, t, t') -> Pi (conv_chk env t, conv_chk (x :: env) t')
  | P.Var  x          ->
    begin
      match find_n x env with
      | Some i -> Bound i
      | None   -> Free (Global x)
    end
  | P.App  (t, t')    -> App (conv_inf env t, conv_chk env t')
  | P.Lam  (_, _)     -> failwith "Lambda is not allowed here"

and conv_chk env t = match t with
  | P.Lam (x, t)      -> Lam (conv_chk (x :: env) t)
  | (P.Ann _ | P.Star | P.Pi _ | P.Var _ | P.App _ ) -> Inf (conv_inf env t)

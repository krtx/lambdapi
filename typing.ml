open Core.Std
module R = Result
open Syntax

type typ = value

type context = (name, typ) List.Assoc.t

type 'a result = ('a, string) R.t

let rec type_inf (i : int) (gamma : context) (t : term_inf) : typ result =
  let open R in
  match t with
  | Ann (e, rho) ->
    type_chk i gamma rho VStar >>= fun _ ->
    let tau = Eval.eval_chk rho [] in
    type_chk i gamma e tau >>= fun _ ->
    return tau
  | Star           -> return VStar
  | Pi (rho, rho') ->
    type_chk i gamma rho VStar >>= fun _ ->
    let tau = Eval.eval_chk rho [] in
    type_chk (i + 1) ((Local i, tau) :: gamma)
             (subst_chk 0 (Free (Local i)) rho') VStar >>= fun _ ->
    return VStar
  | Free x         ->
    begin
      match List.Assoc.find gamma x with
      | Some tau -> return tau
      | None     -> fail "unknown identifier"
    end
  | App (e, e')    ->
    begin
      type_inf i gamma e >>= fun sigma ->
      match sigma with
      | VPi (tau, tau') -> type_chk i gamma e' tau >>= fun _ ->
                           return (tau' (Eval.eval_chk e' []))
      | _               -> fail "illegal application"
    end
  | Bound _        -> fail "bound valiables can't be appeared"

and type_chk (i : int) (gamma : context) (t : term_chk) (typ : typ) : unit result =
  let open R in
  match t, typ with
  | Inf e, typ             -> type_inf i gamma e >>= fun typ' ->
                              if Quote.quote_0 typ <> Quote.quote_0 typ'
                              then fail "type mismatch"
                              else return ()
  | Lam e, VPi (tau, tau') -> type_chk (i + 1) ((Local i, tau) :: gamma)
                                       (subst_chk 0 (Free (Local i)) e)
                                       (tau' (vfree (Local i)))
  | _    , _               -> fail "type mismatch"

and subst_inf (i : int) (r : term_inf) (t : term_inf) : term_inf =
  match t with
  | Ann   (e, tau)    -> Ann (subst_chk i r e, tau)
  | Bound j           -> if i = j then r else Bound j
  | Free  y           -> Free y
  | App   (e, e')     -> App (subst_inf i r e, subst_chk i r e')
  | Star              -> Star
  | Pi    (tau, tau') -> Pi (subst_chk i r tau, subst_chk (i + 1) r tau')

and subst_chk (i : int) (r : term_inf) (t : term_chk) : term_chk =
  match t with
  | Inf e -> Inf (subst_inf i r e)
  | Lam e -> Lam (subst_chk (i + 1) r e)

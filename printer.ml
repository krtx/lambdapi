open Core.Std
open Format

let rec pp_term_inf ppf =
  let open Syntax in function
    | Ann   (t, t') -> fprintf ppf "%a :: %a" pp_term_chk t pp_term_chk t'
    | Star          -> fprintf ppf "*"
    | Pi    (t, t') -> fprintf ppf "forall %a . %a" pp_term_chk t pp_term_chk t'
    | Bound i       -> fprintf ppf "%d" i
    | Free  n       -> fprintf ppf "%a" pp_name n
    | App   (t, t') -> fprintf ppf "%a %a" pp_term_inf t pp_term_chk t'

and pp_term_chk ppf =
  let open Syntax in function
    | Inf t -> fprintf ppf "%a" pp_term_inf t
    | Lam t -> fprintf ppf "fun %a" pp_term_chk t

and pp_name ppf =
  let open Syntax in function
    | Global s -> fprintf ppf "%s" s
    | Local  i -> fprintf ppf "%d" i
    | Quote  i -> fprintf ppf "%d" i

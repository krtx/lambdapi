%token <string> ID
%token ARROW
%token ASTERISK
%token COLONCOLON
%token FORALL
%token FUN
%token LPAREN
%token RPAREN
%token PERIOD
%token EOF

%nonassoc COLONCOLON
%nonassoc ARROW PERIOD

%start <Syntax.Parsing.term option> prog
%type <Syntax.Parsing.term> term
%type <Syntax.Parsing.term> simple_term
%%

prog:
  | EOF      { None }
  | term EOF { Some $1 }
  ;

term:
  | simple_term                           { $1 }
  | ASTERISK                              { Star }
  | FORALL ID COLONCOLON term PERIOD term { Pi ($2, $4, $6) }
  | FUN ID ARROW term                     { Lam ($2, $4) }
  | term COLONCOLON term                  { Ann ($1, $3) }
  | simple_term nonempty_list(simple_term)
      { List.fold_left (fun acc k -> Syntax.Parsing.App (acc, k)) $1 $2 }
  ;

simple_term:
  | ID                                    { Var $1 }
  | x = delimited(LPAREN, term, RPAREN)   { x }

(*
term_inf:
  | LPAREN; e = term_inf; RPAREN             { e }
  | e = term_chk; COLONCOLON; rho = term_chk { Ann (e, rho) }
  | ASTERISK                                 { Star }
  | d = dependent_func                       { d }
  | x = ID                                   { assert false }
  | e1 = term_inf; e2 = term_chk             { App (e1, e2) }

term_chk:
  | t = term_inf                                   { Inf t }
  | FUN; x = ID; t = term_chk                      { assert false }

dependent_func:
  | FORALL; x = ID; dom = term_chk; PERIOD; cod = term_chk
    { assert false }
*)

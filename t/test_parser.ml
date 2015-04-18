
open OUnit2

module P = Parser
module L = Lexer
module S = Syntax.Parsing

let parse s = P.prog L.read (Lexing.from_string s)

let test1 =
  fun _ ->
    assert_equal
      (parse "x")
      (Some S.(Var "x"));
    assert_equal
      (parse "*")
      (Some S.Star);
    assert_equal
      (parse "forall x :: * . x")
      (Some S.(Pi ("x", Star, Var "x")));
    assert_equal
      (parse "fun x -> x")
      (Some S.(Lam ("x", Var "x")));
    assert_equal
      (parse "* :: *")
      (Some S.(Ann (Star, Star)));
    assert_equal
      (parse "x x")
      (Some S.(App (Var "x", Var "x")));
    assert_equal
      (parse "(x)")
      (Some S.(Var "x"))

let test2 =
  fun _ ->
    assert_equal
      (parse "fun x -> fun y -> fun z -> x y z")
      (Some S.(Lam ("x", Lam ("y", Lam ("z", App (App (Var "x", Var "y"), Var "z"))))));
    assert_equal
      (parse "x y z w u v")
      (Some S.(App (App (App (App (App (Var "x", Var "y"), Var "z"), Var "w"), Var "u"), Var "v")));
    assert_equal
      (parse "forall x :: * . x x :: *")
      (Some S.(Ann (Pi ("x", Star, App (Var "x", Var "x")), Star)))

let test_ann =
  fun _ ->
    assert_raises
      P.Error
      (fun _ -> parse "a :: b :: c");
    assert_raises
      P.Error
      (fun _ -> parse "a :: b c :: d");
    assert_equal
      (parse "(a :: b) c :: d")
      (Some S.(Ann (App (Ann (Var "a", Var "b"), Var "c"), Var "d")));
    assert_equal
      (parse "(a :: b) (c :: d)")
      (Some S.(App (Ann (Var "a", Var "b"), Ann (Var "c", Var "d"))))

let suite =
  "Parser test" >:::
  [
    "test1" >:: test1;
    "test2" >:: test2;
    "test_ann" >:: test_ann
  ]

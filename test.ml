
open OUnit2

let _ =
  run_test_tt_main
    ("Test Main" >::: [Test_parser.suite])

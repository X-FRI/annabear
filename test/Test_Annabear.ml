open Alcotest

let () = run "Annabear Tests" [ Test_Parser.tests; Test_Utils.tests ]

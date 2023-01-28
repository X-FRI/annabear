open Annabear.Utils.String
open Alcotest

let test_remaining () =
  (check string)
    "Get the remaining string without first character"
    "ello"
    (remaining "Hello")
;;

let test_remaining_prefix () =
  (check string)
    "Get the remaining string without perfix string"
    "lo"
    (remaining_prefix "Hel" "Hello")
;;

let tests =
  ( "Utils"
  , [ test_case "String.remaining" `Quick test_remaining
    ; test_case "String.remaining_prefix" `Quick test_remaining_prefix
    ] )
;;

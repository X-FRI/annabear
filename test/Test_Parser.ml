open Alcotest
open Annabear.Parser

let ( --> ) f =
  match f () with
  | Ok _ -> true
  | Error msg -> failwith msg
;;

module Test_Char = struct
  open Char

  let test_parse () =
    (check bool) "Success parse" true (( --> ) (fun () -> run (parse 'H') "Hello"))
  ;;

  let test_and_then () =
    (check bool)
      "Success parse"
      true
      (( --> ) (fun () -> run Annabear.Parser.O.(parse 'H' <&> parse 'e') "Hello"))
  ;;

  let test_or_else () =
    (check bool)
      "Success parse"
      true
      (( --> ) (fun () -> run Annabear.Parser.O.(parse 'H' <|> parse 'e') "Hello"))
  ;;

  let test_parse_lowercase () =
    (check bool) "Success parse" true (( --> ) (fun () -> run parse_lowercase "hello"))
  ;;

  let test_parse_uppercase () =
    (check bool) "Success parse" true (( --> ) (fun () -> run parse_uppercase "HELLO"))
  ;;

  let test_parse_digit () =
    (check bool) "Success parse" true (( --> ) (fun () -> run parse_digit "1234567890"))
  ;;

  let test_parse_three_digit_as_str () =
    let parse_three_digit_as_str =
      Annabear.Parser.O.(
        (fun ((c1, c2), c3) -> Core.String.of_char_list [ c1; c2; c3 ])
        <:> (parse_digit <&> parse_digit <&> parse_digit))
    in
    (check bool)
      "Success parse"
      true
      (( --> ) (fun () -> run parse_three_digit_as_str "1234567890"))
  ;;

  let tests =
    [ test_case "Char.parse" `Quick test_parse
    ; test_case "Char.and_then" `Quick test_and_then
    ; test_case "Char.or_else" `Quick test_or_else
    ; test_case "Char.parse_lowercase" `Quick test_parse_lowercase
    ; test_case "Char.parse_uppercase" `Quick test_parse_uppercase
    ; test_case "Char.parse_digit" `Quick test_parse_digit
    ]
  ;;
end

module Test_String = struct
  open String

  let test_parse () =
    (check bool) "Success parse" true (( --> ) (fun () -> run (parse "Hello") "Hello"))
  ;;

  let test_and_then () =
    (check bool)
      "Success parse"
      true
      (( --> ) (fun () -> run Annabear.Parser.O.(parse "Hel" <&> parse "lo") "Hello"))
  ;;

  let test_or_else () =
    (check bool)
      "Success parse"
      true
      (( --> ) (fun () -> run Annabear.Parser.O.(parse "he" <|> parse "He") "Hello"))
  ;;

  let tests =
    [ test_case "String.parse" `Quick test_parse
    ; test_case "String.and_then" `Quick test_and_then
    ; test_case "String.or_else" `Quick test_or_else
    ]
  ;;
end

let tests = "Parser", [ Test_Char.tests; Test_String.tests ] |> List.concat

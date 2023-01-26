open Alcotest
open Annabear

module Test_Utils = struct
  open Utils

  module Test_String = struct
    open String

    let test_remaining () =
      (check string) "Get the remaining string" "ello" (remaining "Hello")
    ;;

    let tests = [ test_case "String.remaining" `Quick test_remaining ]
  end

  let tests = "Utils", [ Test_String.tests ] |> List.concat
end

module Test_Parser = struct
  open Parser

  module Test_Char = struct
    open Char

    let test_parse () =
      (check bool)
        "Success parse"
        true
        (match run (parse 'H') "Hello" with
         | Ok _ -> true
         | Error msg -> failwith msg)
    ;;

    let test_and_then () =
      (check bool)
        "Success parse"
        true
        (match run Parser.O.(parse 'H' &>> parse 'e') "Hello" with
         | Ok _ -> true
         | Error msg -> failwith msg)
    ;;

    let test_or_else () =
      (check bool)
        "Success parse"
        true
        (match run Parser.O.(parse 'H' |>> parse 'e') "Hello" with
         | Ok _ -> true
         | Error msg -> failwith msg)
    ;;

    let tests =
      [ test_case "Char.parse" `Quick test_parse
      ; test_case "Char.and_then" `Quick test_and_then
      ; test_case "Char.or_else" `Quick test_or_else
      ]
    ;;
  end

  let tests = "Parser", [ Test_Char.tests ] |> List.concat
end

let () = run "Annabear Tests" [ Test_Parser.tests; Test_Utils.tests ]

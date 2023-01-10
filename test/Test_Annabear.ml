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

    let test_parse_success () =
      (check bool)
        "Success parse"
        true
        (match parse 'H' |> fun parser -> parser "Hello" with
         | Ok ('H', "ello") -> true
         | Ok (char_to_match, remaining) ->
           failwith (Format.sprintf "Test Error : (%c, %s)" char_to_match remaining)
         | Error msg -> failwith msg)
    ;;

    let test_parse_failure () =
      (check bool)
        "Failure parse"
        true
        (match parse 'A' |> fun parser -> parser "Hello" with
         | Ok (char_to_match, remaining) ->
           failwith (Format.sprintf "Test Error : (%c, %s)" char_to_match remaining)
         | Error _ -> true)
    ;;

    let tests =
      [ test_case "Char.parse" `Quick test_parse_success
      ; test_case "Char.parse" `Quick test_parse_failure
      ]
    ;;
  end

  let tests = "Parser", [ Test_Char.tests ] |> List.concat
end

let () = run "Annabear Tests" [ Test_Parser.tests; Test_Utils.tests ]

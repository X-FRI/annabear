open Core
open Annabear
open Alcotest

let test_parse_char () =
  let parse_a = Parsers.parse_char 'A' in
  let _success_parse =
    match Parsers.run parse_a "ABC" with
    | Success _ -> ()
    | Failure msg -> failwith msg
  in
  let _failure_parse =
    match Parsers.run parse_a "BC" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let test_and_then () =
  let parse_a = Parsers.parse_char 'A' in
  let parse_b = Parsers.parse_char 'B' in
  let _success_parse =
    match Parsers.run (Combinators.and_then parse_a parse_b) "ABC" with
    | Success _ -> ()
    | Failure msg -> failwith msg
  in
  let _failure_parse =
    match Parsers.run (Combinators.and_then parse_a parse_b) "CBA" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let test_or_else () =
  let parse_a = Parsers.parse_char 'A' in
  let parse_b = Parsers.parse_char 'B' in
  let _success_parse =
    let _ =
      match Parsers.run (Combinators.or_else parse_a parse_b) "ABC" with
      | Success _ -> ()
      | Failure msg -> failwith msg
    in
    let _ =
      match Parsers.run (Combinators.or_else parse_a parse_b) "BAC" with
      | Success _ -> ()
      | Failure msg -> failwith msg
    in
    ()
  in
  let _failure_parse =
    match Parsers.run (Combinators.or_else parse_a parse_b) "CBA" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let test_any_of () =
  let _parse_lower_case =
    match
      Parsers.run
        (Combinators.any_of
           (List.init 26 ~f:(fun n -> Char.of_int (n + 97) |> Option.value_exn)))
        "aBC"
    with
    | Success (c, r) ->
      if Char.(c = 'a') && String.equal r "BC" then () else failwith "_failure_parse"
    | Failure msg -> failwith msg
  in
  let _parse_lower_case_failure =
    match
      Parsers.run
        (Combinators.any_of
           (List.init 26 ~f:(fun n -> Char.of_int (n + 97) |> Option.value_exn)))
        "ABC"
    with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  let _parse_digit =
    match
      Parsers.run
        (Combinators.any_of
           (List.init 10 ~f:(fun n -> Char.of_int (n + 48) |> Option.value_exn)))
        "1aBC"
    with
    | Success (c, r) ->
      if Char.(c = '1') && String.equal r "aBC" then () else failwith "_failure_parse"
    | Failure msg -> failwith msg
  in
  let _parse_digit_failure =
    match
      Parsers.run
        (Combinators.any_of
           (List.init 10 ~f:(fun n -> Char.of_int (n + 48) |> Option.value_exn)))
        "|ABC"
    with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let _ =
  run
    "Annabear"
    [ ( "combinators"
      , [ test_case "parse_char" `Quick test_parse_char
        ; test_case "and_then" `Quick test_and_then
        ; test_case "or_else" `Quick test_or_else
        ; test_case "any_of" `Quick test_any_of
        ] )
    ]
;;

open Core
open Annabear
open Alcotest

let test_parse_char () =
  let parse_a = Parsers.parse_char 'A' in
  let _success_parse =
    match Utils.run parse_a "ABC" with
    | Success _ -> ()
    | Failure msg -> failwith msg
  in
  let _failure_parse =
    match Utils.run parse_a "BC" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let test_and_then () =
  let parse_a = Parsers.parse_char 'A' in
  let parse_b = Parsers.parse_char 'B' in
  let _success_parse =
    match Utils.run (Combinators.and_then parse_a parse_b) "ABC" with
    | Success (('A', 'B'), "C") -> ()
    | Failure msg -> failwith msg
    | _ -> failwith "_failure_parse"
  in
  let _failure_parse =
    match Utils.run (Combinators.and_then parse_a parse_b) "CBA" with
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
      match Utils.run (Combinators.or_else parse_a parse_b) "ABC" with
      | Success _ -> ()
      | Failure msg -> failwith msg
    in
    let _ =
      match Utils.run (Combinators.or_else parse_a parse_b) "BAC" with
      | Success _ -> ()
      | Failure msg -> failwith msg
    in
    ()
  in
  let _failure_parse =
    match Utils.run (Combinators.or_else parse_a parse_b) "CBA" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let test_any_of () =
  let _parse_lower_case =
    match
      Utils.run
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
      Utils.run
        (Combinators.any_of
           (List.init 26 ~f:(fun n -> Char.of_int (n + 97) |> Option.value_exn)))
        "ABC"
    with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  let _parse_digit =
    match
      Utils.run
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
      Utils.run
        (Combinators.any_of
           (List.init 10 ~f:(fun n -> Char.of_int (n + 48) |> Option.value_exn)))
        "|ABC"
    with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> ()
  in
  ()
;;

let test_map () =
  match
    Utils.run
      Combinators.O.(
        Parsers.parse_digit
        >>> Parsers.parse_digit
        >>> Parsers.parse_digit
        |-> (fun ((c1, c2), c3) -> String.of_char_list [ c1; c2; c3 ])
        |-> Int.of_string)
      "123A"
  with
  | Success (123, "A") -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "_failure_parse"
;;

let test_sequence () =
  match
    Utils.run
      (Combinators.sequence
         [ Parsers.parse_char 'a'; Parsers.parse_char 'b'; Parsers.parse_char 'c' ])
      "abcd"
  with
  | Success ([ 'a'; 'b'; 'c' ], "d") -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "_failure_parse"
;;

let test_parse_string () =
  match Utils.run (Parsers.parse_string "ABC") "ABCDE" with
  | Success ("ABC", "DE") -> ()
  | Failure msg -> failwith msg
  | _ -> failwith "_failure_parse"
;;

let _ =
  run
    "Annabear"
    [ ( "combinators"
      , [ test_case "parse_char" `Quick test_parse_char
        ; test_case "and_then" `Quick test_and_then
        ; test_case "or_else" `Quick test_or_else
        ; test_case "any_of" `Quick test_any_of
        ; test_case "map" `Quick test_map
        ; test_case "sequence" `Quick test_sequence
        ; test_case "parse string" `Quick test_parse_string
        ] )
    ]
;;

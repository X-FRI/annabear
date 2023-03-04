open Annabear
open Omtl

let test_parse_char () =
  let parse_a = Parsers.parse_char 'A' in
  let _success_parse =
    match Parsers.run parse_a "ABC" with
    | Success _ -> OK
    | Failure msg -> failwith msg
  in
  let _failure_parse =
    match Parsers.run parse_a "BC" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> OK
  in
  OK
;;

let test_and_then () =
  let parse_a = Parsers.parse_char 'A' in
  let parse_b = Parsers.parse_char 'B' in
  let _success_parse =
    match Parsers.run (Combinators.and_then parse_a parse_b) "ABC" with
    | Success _ -> OK
    | Failure msg -> failwith msg
  in
  let _failure_parse =
    match Parsers.run (Combinators.and_then parse_a parse_b) "CBA" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> OK
  in
  OK
;;

let test_or_else () =
  let parse_a = Parsers.parse_char 'A' in
  let parse_b = Parsers.parse_char 'B' in
  let _success_parse =
    let _ =
      match Parsers.run (Combinators.or_else parse_a parse_b) "ABC" with
      | Success _ -> OK
      | Failure msg -> failwith msg
    in
    let _ =
      match Parsers.run (Combinators.or_else parse_a parse_b) "BAC" with
      | Success _ -> OK
      | Failure msg -> failwith msg
    in
    OK
  in
  let _failure_parse =
    match Parsers.run (Combinators.or_else parse_a parse_b) "CBA" with
    | Success _ -> failwith "_failure_parse"
    | Failure _ -> OK
  in
  OK
;;

let _ =
  "Annabear"
  +:> [ "Test parse_char" >== test_parse_char
      ; "Test and_then" >== test_and_then
      ; "Test or_else" >== test_or_else
      ]
  |> run ~color:true
;;

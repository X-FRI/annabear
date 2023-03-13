(**********************************************************************************)
(* The MIT License (MIT)                                                          *)
(*                                                                                *)
(* Copyright (c) 2022 Muqiu Han                                                   *)
(*                                                                                *)
(* Permission is hereby granted, free of charge, to any person obtaining a copy   *)
(* of this software and associated documentation files (the "Software"), to deal  *)
(* in the Software without restriction, including without limitation the rights   *)
(* to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *)
(* copies of the Software, and to permit persons to whom the Software is          *)
(* furnished to do so, subject to the following conditions:                       *)
(*                                                                                *)
(* The above copyright notice and this permission notice shall be included in all *)
(* copies or substantial portions of the Software.                                *)
(*                                                                                *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *)
(* AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *)
(* OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *)
(* SOFTWARE.                                                                      *)
(**********************************************************************************)

open Types
open Core
open Utils

let and_then x y =
  let inner input =
    match run x input with
    | Failure err -> Failure err
    | Success (x_result, x_remaining) ->
      (match run y x_remaining with
       | Failure err -> Failure err
       | Success (y_result, y_remaining) -> Success ((x_result, y_result), y_remaining))
  in
  Parser inner
;;

let or_else x y =
  let inner input =
    match run x input with
    | Failure _ -> run y input
    | Success x_result -> Success x_result
  in
  Parser inner
;;

let choise x = List.reduce_exn x ~f:or_else
let any_of chars = List.map ~f:parse_char chars |> choise

let map ~f x =
  let inner input =
    match run x input with
    | Success (value, remaining) -> Success (f value, remaining)
    | Failure err -> Failure err
  in
  Parser inner
;;

let return x =
  let inner input = Success (x, input) in
  Parser inner
;;

let apply ~f x = and_then f x |> map ~f:(fun (f, x) -> f x)

module O = struct
  let ( >>> ) = and_then
  let ( <|> ) = or_else
  let ( <-> ) f x = map ~f x
  let ( |-> ) x f = map ~f x
  let ( --> ) f x = apply ~f x
end

let lift2 f x y = O.(return f --> x --> y)

module Int = struct
  let add = lift2 ( + )
end

module String = struct
  let starts_with = lift2 (fun prefix str -> String.is_prefix ~prefix str)
end

let rec sequence parsers =
  let cons = lift2 (fun h t -> h :: t) in
  match parsers with
  | [] -> return []
  | h :: t -> cons h (sequence t)
;;

let rec parse_zero_or_more x input =
  match run x input with
  | Failure _ -> [], input
  | Success (h, t) ->
    let subsequent, remaining = parse_zero_or_more x t in
    h :: subsequent, remaining
;;

let many parser =
  let inner input = Success (parse_zero_or_more parser input) in
  Parser inner
;;

let many1 parser =
  let inner input =
    match Utils.run parser input with
    | Failure err -> Failure err
    | Success (h, t) ->
      let subsequent, remaining = parse_zero_or_more parser t in
      Success (h :: subsequent, remaining)
  in
  Parser inner
;;
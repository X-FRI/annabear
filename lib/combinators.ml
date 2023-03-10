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
open Parsers
open Core

let and_then (x : 'a parser) (y : 'a parser) : ('a * 'a) parser =
  let inner (input : string) : (('a * 'a) * string) parse_result =
    match run x input with
    | Failure err -> Failure err
    | Success (x_result, x_remaining) ->
      (match run y x_remaining with
       | Failure err -> Failure err
       | Success (y_result, y_remaining) -> Success ((x_result, y_result), y_remaining))
  in
  Parser inner
;;

let or_else (x : 'a parser) (y : 'a parser) : 'a parser =
  let inner (input : string) : ('a * string) parse_result =
    match run x input with
    | Failure _ -> run y input
    | Success x_result -> Success x_result
  in
  Parser inner
;;

let choise (parsers : 'a parser list) : 'a parser = List.reduce_exn parsers ~f:or_else
let any_of (chars : char list) : 'a parser = List.map ~f:parse_char chars |> choise

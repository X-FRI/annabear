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

open Core
open Utils

let parse_digit =
  Combinators.any_of (List.init 10 ~f:(fun n -> Char.of_int (n + 48) |> Option.value_exn))
;;

let parse_string str =
  str
  |> String.to_list
  |> List.map ~f:parse_char
  |> Combinators.sequence
  |> Combinators.map ~f:String.of_char_list
;;

let parse_int =
  Combinators.and_then
    (Combinators.option (parse_char '-'))
    (Combinators.any_of
       (List.init 10 ~f:(fun n -> Char.of_int (n + 48) |> Option.value_exn))
    |> Combinators.many1)
  |> Combinators.map ~f:(fun (sign, digit_list) ->
       let digit = digit_list |> String.of_char_list |> Int.of_string in
       match sign with
       | Some _ -> -digit
       | None -> digit)
;;

let parse_char = parse_char

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
open Types

module String = struct
  include String

  (** Gets the first character of a string
      and returns a string containing the remaining characters.
      E.g: let (first_char, remaining) = remaining "foo" *)
  let remaining (str : string) : char * string =
    match str |> to_list with
    | [ last_char ] -> last_char, ""
    | first_char :: remaining -> first_char, remaining |> of_char_list
    | _ -> failwith "String is empty!"
  ;;
end

let parse_char char_to_match =
  let inner str =
    if String.is_empty str
    then Failure "No more input"
    else (
      let first_char, remaining = String.remaining str in
      if Char.equal first_char char_to_match
      then Success (char_to_match, remaining)
      else Failure (Format.sprintf "Expecting '%c'. Got '%c'" char_to_match first_char))
  in
  Parser inner
;;

let run (parser : 'a parser) (input : string) : ('a * string) parse_result =
  let (Parser inner) = parser in
  inner input
;;
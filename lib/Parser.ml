(**********************************************************************************)
(* MIT License                                                                    *)
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

type ('a, 'b) parser = Parser of ('b -> ('a, 'b) Result.t)

let run (parser : ('a, 'b) parser) (input : 'b) : ('a, 'b) Result.t =
  let (Parser inner_fn) = parser in
  inner_fn input
;;

let and_then (parser_a : ('a, 'b) parser) (parser_b : ('c, 'b) parser)
  : ('a * 'c, 'b) parser
  =
  let inner_fn input =
    match run parser_a input with
    | Error err -> Error err
    | Ok (value_a, remaining_a) ->
      (match run parser_b remaining_a with
       | Error err -> Error err
       | Ok (value_b, remaining_b) -> Ok ((value_a, value_b), remaining_b))
  in
  Parser inner_fn
;;

let or_else (parser_a : ('a, 'b) parser) (parser_b : ('a, 'b) parser) : ('a, 'b) parser =
  let inner_fn input =
    match run parser_a input with
    | Ok result -> Ok result
    | Error _ -> run parser_b input
  in
  Parser inner_fn
;;

let choise (list_of_parsers : ('a, 'b) parser list) : ('a, 'b) parser =
  Core.List.reduce_exn ~f:or_else list_of_parsers
;;

let any_of (list_of_datas : 'a list) (data_parse_fn : 'a -> ('a, 'b) parser)
  : ('a, 'b) parser
  =
  list_of_datas |> Core.List.map ~f:data_parse_fn |> choise
;;

let map (f : 'a -> 'b) (parser : ('c, 'd) parser) : ('e, 'f) parser =
  let inner_fn input =
    match run parser input with
    | Ok (value, remaining) -> Ok (f value, remaining)
    | Error msg -> Error msg
  in
  Parser inner_fn
;;

module O = struct
  let ( <&> ) = and_then
  let ( <|> ) = or_else
  let ( >>= ) = any_of
  let ( <:> ) = map
  let ( |>> ) (x : ('c, 'd) parser) (f : 'a -> 'b) : ('e, 'f) parser = map f x
end

module Char = struct
  open O
  open Utils
  include Core.Char

  type result = char
  type data = string

  let parse (char_to_match : result) : (result, data) parser =
    let inner_fn (str : data) : (result, data) Result.t =
      if String.is_empty str
      then Error "No more input"
      else if String.get str 0 |> equal char_to_match
      then Ok (char_to_match, String.remaining str)
      else
        Error (Format.sprintf "Expecting '%c'. Got '%c'" char_to_match (String.get str 0))
    in
    Parser inner_fn
  ;;

  let parse_lowercase : ('a, 'b) parser = Char.lowercase_char_list >>= parse
  let parse_uppercase : ('a, 'b) parser = Char.uppercase_char_list >>= parse
  let parse_digit : ('a, 'b) parser = Char.digit_char_list >>= parse
end

module String = struct
  include Core.String
  open Utils

  type result = string
  type data = string

  let parse (str_to_match : result) : (result, data) parser =
    let inner_fn (str : data) : (result, data) Result.t =
      if String.is_empty str
      then Error "No more input"
      else if String.is_prefix str ~prefix:str_to_match
      then Ok (str_to_match, String.remaining_prefix str_to_match str)
      else Error (Format.sprintf "Expecting '%s'. Got '%s'" str_to_match str)
    in
    Parser inner_fn
  ;;
end

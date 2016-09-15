(************************************************************************)
(*                              NLTC                                    *)
(* - A natural language text comparator written in OCaml                *)
(* Copyright (C) 2016  Claes Worm                                       *)
(*                                                                      *)
(* This program is free software: you can redistribute it and/or modify *)
(* it under the terms of the GNU General Public License as published by *)
(* the Free Software Foundation, either version 3 of the License, or    *)
(* (at your option) any later version.                                  *)
(*                                                                      *)
(* This program is distributed in the hope that it will be useful,      *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of       *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *)
(* GNU General Public License for more details.                         *)
(*                                                                      *)
(* You should have received a copy of the GNU General Public License    *)
(* along with this program.  If not, see <http://www.gnu.org/licenses/>.*)
(************************************************************************)

(** Type of tokens that get's added to the document comparator-sets *)

(*
type token_type = 
  | Word
  | Word_para
  | Num
  | Alnumdot
  | Pararef
  | Email
  [@@deriving ord]

type 'a token =

(*< is this practical at all, as we can't save diff types inside..*)
*)  

type token = 
  | Word of string
  | Num of string
  | Alnumdot of string
  | Pararef of string * string
  | Email of string

(*<gomaybe change this type -> see example:

  type a = A | B
  type 'a foo = a * 'a

  let a : string foo = A, "koq" 
  and b : int foo = B, 2 ;;
  val a : bytes foo = (A, "koq")
  val b : int foo = (B, 2)
*)
(*<goto extend with ppx [ord; show; eq?; yojson?; ]
*)

type t = token

let to_tstring = function 
  | Word s       -> "Word(" ^ s ^ ")"
  | Num s        -> "Num(" ^ s ^ ")"
  | Alnumdot s   -> "Alnumdot(" ^ s ^ ")"
  | Email s      -> "Email(" ^ s ^ ")"
  | Pararef (s,s') -> "Pararef(" ^s^ " " ^s'^ ")"

let to_tstring_anon = function 
  | Word s
  | Num s      
  | Alnumdot s 
  | Email s      -> s
  | Pararef (s,s') -> s^" "^s'

let to_string = to_tstring_anon

let type_string = function
  | Word _ -> "Word"
  | Num _ -> "Num"
  | Alnumdot _ -> "AlphaNum"
  | Email _ -> "Email"
  | Pararef (s,s') -> "ParagraphRef"

let to_json_aux to_str_fun ts = 
  let module Y = Yojson.Basic in
  (`Assoc [ 
    ("tokens",
     (`List (List.map (fun t -> 
       `String (to_str_fun t)
      ) ts ))) ])
  |> Y.pretty_to_string

let to_json = 
  to_json_aux to_tstring

let to_json_anon = 
  to_json_aux to_tstring_anon



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


(**This module add's 'Batteries Included' functionality to the Ocaml-text module*)
include Text

(**Module inspired by batteries included implementation of enumerable String*)
let enum s =   
  let l = Text.length s in
  let rec make i =
    BatEnum.make
      ~next:(fun () ->
        if !i = l then
          raise BatEnum.No_more_elements
        else
          Text.get s (BatRef.post_incr i)
      )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (BatRef.copy i))
  in
  make (ref 0)

  (**Slower than working with a normal latin-string enumeration, as we don't use mutation*)
let of_enum e = BatList.of_enum e |> BatString.concat ""

let to_stream s = 
  match check s with
  | None -> 
    Stream.from (fun i -> 
      if i >= length s then None 
      else Some (get s i))
  | Some err -> 
    ((print_endline ("\nUnicode-validation error:\n"^err^"\n")); 
     [< >])

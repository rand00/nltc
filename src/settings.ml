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


type analysis_settings = {
  mode : [`Strict | `Loose]; 
  exclude_strings : string list;
  (*> only possible with cmp_with_lists (n^2). Int means how many words
    max can be between matches*)
  (*> goto is this going to be used?*)
  words_in_seq_maxdist : int option; 
}

(**Std. analysis settings*)
(*The analysis is at the curren point in time not making full usage of
  these settings - these are implemented to be able to easily extend
  the module with the wished-for functionality*)

let std_analysis = {
  mode = `Loose;
  (*> goto implement and use*)
  exclude_strings = [];
  words_in_seq_maxdist = None;
} 

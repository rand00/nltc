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


(** //// parser helper fun's *)
(*compiled regexps*)
let rx_spcc = Pcre.regexp " "
let rx_comma = Pcre.regexp ","
let rx_dash = Pcre.regexp "-"
let rx_semic = Pcre.regexp ";"
let rx_tab = Pcre.regexp "\t"
let rx_sntc_end = Pcre.regexp "[\n\r.]"
let rx_wrdc_accent = Pcre.regexp_or 
  ["á"; "ć"; "é"; "ǵ"; "í"; "ḱ"; "ĺ"; "ḿ"; "ń"; "ó"; 
   "ŕ"; "ś"; "ú"; "ǘ"; "ý"; "ź"; "ǽ"; "ǿ"; "ǻ"; "à";
   "è"; "ì"; "ǹ"; "ò"; "ù"; "ǜ"; "ỳ"]

(**char-matching fun's - pcre / ocamltext *)
let is_graph = Text.is_graph (*matches all visual chars*)
let is_symb s = if not (Text.is_alpha s) then is_graph s else false
let is_dash = Pcre.pmatch ~rex:rx_dash
let is_semic = Pcre.pmatch ~rex:rx_semic

let is_spcc = Pcre.pmatch ~rex:rx_spcc
let is_comma = Pcre.pmatch ~rex:rx_comma 
let is_tab = Pcre.pmatch ~rex:rx_tab

(*goto: find solution to check for several spaces / weird chars*)
let is_sntc_end = Pcre.pmatch ~rex:rx_sntc_end 

let is_wrdc_wrd = Text.is_alpha
(*let is_wrdc_vis = Pcre.pmatch ~rex:rx_wrdc_vis *)
let is_upper = Text.is_upper
let is_lower = Text.is_lower
let is_wrdc_accent = Pcre.pmatch ~rex:rx_wrdc_accent
let is_digit = Text.is_digit

(**word-matching fun's*)
let is_noun s = Text.(
  let len = length s in
  if len < 2 then false else
    if is_upper (get s 0) then
      is_lower (sub s 1 (len-1))
    else false)

(**stats updating fun's*)
let s_upd_chr c s = begin
  s.c.amount_wrd <- succ s.c.amount_wrd;
  s.c.amount_vis <- succ s.c.amount_vis;
  if is_upper c then s.c.t_upper.amount <- succ s.c.t_upper.amount;
  s.w.len_curr <- succ s.w.len_curr;
  s
end

let s_upd_wrd ch_acc s = begin
  s.w.amount <- succ s.w.amount;
  if s.w.len_curr > s.w.longest then s.w.longest <- s.w.len_curr;
  if is_noun ch_acc then s.w.t_noun.amount <- succ s.w.t_noun.amount;
  if is_upper ch_acc then s.w.t_upper.amount <- succ s.w.t_upper.amount;
  s.w.len_curr <- 0;
  s.s.len_curr <- succ s.s.len_curr;
  s
end

let s_upd_snt s = begin
  s.s.amount <- succ s.s.amount;
  if s.s.len_curr > s.s.longest then s.s.longest <- s.s.len_curr;
  s.s.len_curr <- 0;
  s
end

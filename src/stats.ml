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


open Batteries

type stats_settings = {
  (*goo*)
}

(**Stats pr. each sentence-comparison; this gets put in db together
   with list of words that matched; when uploading stat's to db
   (possibly with callback), calculate interesting stats from a mix of
   the matched words and these stats.*)
(*save this in tuple in combine_all2i together with words-matched*)
type sntc2_stats = {
  n_words1 : int;
  n_words2 : int;
  len_words1 : int;
  len_words2 : int;

  (*weighted match-wellness*)
  mwellness : float;
}


(*maybe > 
  . add list of words here? (or in sntc2_stats)
  . add id's txt, sntc, (words)
*)
(**Stats pr. each text-comparison. Gets updated on the fly by
   Stats-function called in [Analysis.combine_all2i]*)
type text2_stats = {
  n_sntc : int;
  n_msntc : int;
  pct_msntc : float;

  (*max # of words that could be matched (the sum of all the smallest
    sntcs in comparisons) *)
  n_mwords_max : int; 
  n_mwords : int;
  pct_words_match : float;

  max_mwords_sntc : int;
  avg_mwords_sntc : float;

  n_msntc_over_min : int;
  pct_msntc_over_min : float;
  
}

type proto_stats = {
  
  
}

(*goo*)
let sntc2stat 

(*[stats] arg: liste med stats fra hver sætning-sammenligning (også inkluderet ord?)*)
let text2_finalize stats  = ()

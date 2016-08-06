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
open Token


type word_settings = {
  (*how equal two strings must be*)
  goal : float;
  (*weight of the percentage of char's that need to be equal*)
  weight_eq : float;
  (*weight of the percentage of end-char's that need to be equal*)
  weight_endeq : float;
  (*weight of the importance of length of words in the comparison
    ! range: 0.0 -> 1.0 *)
  weight_wordlen : float;
  (*length of end-letters compared differently*)
  endeq_len : int;
  (*the minimum length of words that are to be compared by word length*)
  mineq_len : int;
  lowercase_convert : bool;
  (*toggles a special comparison where certain letters are seen as equal 
    -> fx '!' and 'l'/'i'*)
(*  ocr_lookalike : bool;*)
}

let str_of_wset wset = 
  let s = Printf.sprintf in
  String.concat "\n" [
    s "%-16s %.4f" "goal:" wset.goal;
    s "%-16s %.4f" "weight_eq:" wset.weight_eq;
    s "%-16s %.4f" "weight_endeq:" wset.weight_endeq;
    s "%-16s %.4f" "weight_wordlen:" wset.weight_wordlen;
    s "%-16s %d" "mineq_len:" wset.mineq_len;
    s "%-16s %d" "endeq_len:" wset.endeq_len;
    s "%-16s %b" "lowercase_convert:" wset.lowercase_convert;
  ]


(**The type of settings for tokens - contains word_settings.  For
   now we only use these for the equals function (which does not
   rely on the same comparison as [compare]!)*)
type token_settings = {
  word_sett : word_settings;
}

module WordCmpLoose = struct
(**this datatype is used as input to [cmp_settings] to weight the token analysis
   in a user-defined way
   - word_len    -> set's the weight of token Word's length
*)
  type t = string


(**Laissez faire settings*)
  let wset = {
    goal = 0.85;
    weight_eq = 1.0;
    weight_endeq = 0.1; 
    weight_wordlen = 0.6; 
    endeq_len = 2;
    mineq_len = 3; 
    lowercase_convert = true;
  } 


(**Laissez faire settings*)
  let wset3 = {
    goal = 0.9;
    weight_eq = 1.0;
    weight_endeq = 0.4; 
    weight_wordlen = 0.6; 
    endeq_len = 2;
    mineq_len = 3; 
    lowercase_convert = true;
  } 

(** Oopalot settings - more stringent*)
  let wset4 = {
    goal = 0.9; 
    weight_eq = 1.; 
    weight_endeq = 0.7; 
    weight_wordlen = 0.9;
    endeq_len = 2; 
    mineq_len = 3; 
    lowercase_convert = true;
  }


(** Max-strict comparison *)
  let wset5 = {
    goal = 1.0; 
    weight_eq = 1.; 
    weight_endeq = 0.0; 
    weight_wordlen = 1.0;
    endeq_len = 0; 
    mineq_len = 0; 
    lowercase_convert = true;
  }

(**Functions relevant for boolean arrays 
   - we'll use these for an extended version of the hammock-distance algorithm*)
  let calc_eqpct arr = 
    let count_n_eq = Array.fold_left (fun acc e -> 
      if e then succ acc else acc
    ) 0
    in match Array.length arr with
    | 0 -> 0.
    | _ -> (Float.of_int (count_n_eq arr)) /. (Float.of_int (Array.length arr))


  (**Problem-specific comparison-function for unicode-text that
     enables a more loose text-comparison, better suited for OCR
     text.*)
  (*special equal start (:search keyword for evaluation in REPL)*)
  let equal ?(verbose=false) ~settings s1 s2 = 
    let open Otext in
    let s1, s2 = 
      if settings.lowercase_convert then 
        (lower s1), (lower s2)
      else
        s1, s2
    and len_s1 = length s1 
    and len_s2 = length s2 
    in
    let len_cmp,len_max = 
      if len_s1 < len_s2 then 
        len_s1, len_s2
      else 
        len_s2, len_s1
    in
    if verbose then begin 
      Printf.printf "s1 = %s, s2 = %s\n" s1 s2;
      Printf.printf "%-16s %d\n" "len_cmp:" len_cmp;
      Printf.printf "%-16s %d\n" "len_max:" len_max;
    end;
    
    let eqarr = Array.make len_cmp false 
    in Enum.iter2i (fun i s1_c s2_c -> 
      eqarr.(i) <- ((Otext.compare s1_c s2_c) = 0)
    ) (enum s1) (enum s2);
    
  (*mineq_len dominates endeq_len*)
    if len_cmp <= settings.mineq_len then 

      if len_max = 0 then 
        true, 1. (*empty strings are equal*)
      else if len_cmp = 0 then
        false, 0. (*if one string is empty, and the other is not*)
      else (*when we don't take endeq_len and weight_endeq into account*)
        
        let eq_pct_hd = calc_eqpct eqarr 

        (* > goto goo use all weights and eq'parts correctly instead of this
           :   (eq_pct1 (?map s-curve?) * weight1 + eq_pctN (?map s-curve?) * weightN) 
               / sum weight(1 -- N)
        *)
        and eq_pct_wordlen =  (Float.of_int len_cmp) /. (Float.of_int len_max)
        in 
        (*inverted weighted word-length equality*)
        let invert_weq_wordlen = (1.0 -. eq_pct_wordlen) *. settings.weight_wordlen in
        (*when there is only one part of a word to compare - no need for weights on word*)
        let eq_pct_final = eq_pct_hd -. invert_weq_wordlen
        in

        if verbose then begin
          Printf.printf "%-16s %.4f\n" "eq_pct_hd:" eq_pct_hd;
          Printf.printf "%-16s %.4f\n" "invert_weq_wordlen:" invert_weq_wordlen;
          Printf.printf "%-16s %.4f\n" "eq_pct_final:" eq_pct_final;
          Printf.printf "%-16s %.4f\n" "settings.goal:" settings.goal;
        end;
        let success = eq_pct_final >= settings.goal
        in success, eq_pct_final

    else (*when length of strings are bigger than mineq_len*)
      
    (*defining the bounds of comparison*)
      let len_over_min = len_cmp - settings.mineq_len in
      let len_middle = len_over_min - settings.endeq_len 
      in
      let len_cmp_hd, len_cmp_tl = 
        if len_middle >= 0 then 
          (settings.mineq_len + len_middle), 
          (settings.endeq_len)
        else 
          (settings.mineq_len), 
          (settings.endeq_len + len_middle)
      in

      if verbose then begin
        Printf.printf "%-16s %d\n" "len_cmp_hd:" len_cmp_hd;
        Printf.printf "%-16s %d\n" "len_cmp_tl:" len_cmp_tl;
      end;
      
      let eq_pct_hd = calc_eqpct (Array.left  eqarr len_cmp_hd)
      and eq_pct_tl = calc_eqpct (Array.right eqarr len_cmp_tl)
    (*word length difference*)
      and eq_pct_wordlen =  (Float.of_int len_cmp) /. (Float.of_int len_max)
      in

      if verbose then begin
        Printf.printf "%-16s %.4f\n" "eq_pct_hd:" eq_pct_hd;
        Printf.printf "%-16s %.4f\n" "eq_pct_tl:" eq_pct_tl;
        Printf.printf "%-16s %.4f\n" "eq_pct_wordlen:" eq_pct_wordlen;
      end;

      let weighted_eq_max = (+.) 
        settings.weight_eq
        (if len_cmp_tl = 0 then 0. else settings.weight_endeq)
      in
      (*equality taking weights into account*)
      let weq_hd = eq_pct_hd *. settings.weight_eq 
      and weq_tl = eq_pct_tl *. settings.weight_endeq

      (*inverted weighted word-length equality*)
      and invert_weq_wordlen = (1.0 -. eq_pct_wordlen) *. settings.weight_wordlen 
      in
      (*final weighted equality converted to pct*)
      let eq_pct_final = ((weq_hd +. weq_tl) /. weighted_eq_max) -. invert_weq_wordlen
      in
      begin
        if verbose then begin
          Printf.printf "%-16s %.4f\n" "weighted_eq_max:" weighted_eq_max;
          Printf.printf "%-16s %.4f\n" "weq_hd:" weq_hd;
          Printf.printf "%-16s %.4f\n" "weq_tl:" weq_tl;
          Printf.printf "%-16s %.4f\n" "invert_weq_wordlen:" invert_weq_wordlen;
          Printf.printf "%-16s %.4f\n" "eq_pct_final:" eq_pct_final;
          Printf.printf "%-16s %.4f\n" "settings.goal:" settings.goal;
        end
      end;
      let success = eq_pct_final >= settings.goal
      in success, eq_pct_final


end

(**Std TokenSet-settings if no settings-argument is passed to [run] *)  
let std_cmp_settings = { word_sett = WordCmpLoose.wset }


(*
module WordSet = Set.Make (struct 
  type t = WordCmp.t
    (**The [WordCmp.equal] function get's partially applied with the current settings*)
  let equal = WordCmp.equal 
    ~verbose:false 
    ~settings:WordCmp.wset3
end)
*)

(**This is the loosely ordered type of Tokens - not suited for Set's -
   therefore there is no [compare] function implemented here*)
module TokenCmpLoose = struct

  (**type t now equals Token.t*)
  include Token 

  (**The curried equal-function that we partially apply with settings before analysis
     - for now this function only equals content of tokens as strings, 
       and it does not cross-compare between types of tokens, other than word-types*)
  (*goto implement different equal settings for different types of tokens...*)
  (*goto remember to extend this func if we extend the types of tokens*)
  let equal ?(verbose=false) ~settings t1 t2 = 
    let equal = 
      WordCmpLoose.equal 
        ~verbose ~settings:settings.word_sett 
    in match t1,t2 with
    | Word s1     , Word s2 
    | Word_para s1, Word_para s2
    | Word s1     , Word_para s2
    | Word_para s1, Word s2
    | Num s1      , Num s2 
    | Alnumdot s1 , Alnumdot s2
    | Email s1    , Email s2 
    | Pararef (_, s1), Pararef (_, s2) -> equal s1 s2
    | _ -> false, 0.


end


(**This is the strictly ordered type - compatible with Set's*)
module TokenCmpStrict = struct

  (**type t now equals Token.t*)
  include Token 

  (**Helper functions for the [compare] function that compares by a
     mix of string and type-rank comparison*)
  (*note: if I ever need to extend this func - look at all compare func's in this scope*)
  let find_rank = function
    | Word _ -> 0
    | Word_para _ -> 1
    | Pararef _ -> 2
    | Num _ -> 3
    | Alnumdot _ -> 4
    | Email _ -> 5

  let compare_typerank t1 t2 = Int.compare (find_rank t1) (find_rank t2)

  (**Comparison function of tokens, for use for TokenSet's *)
  (*Implement later; OCR-letters option => fx "i"/"l"/"!" are equal*)
  let compare ~lowercase_conv t1 t2 = match t1,t2 with
    (*Word and Word_para are seen as equal types here*)
    | Word s1     , Word s2 
    | Word_para s1, Word_para s2
    | Word s1     , Word_para s2
    | Word_para s1, Word s2
    (*The following types don't mix with others*)
    | Num s1      , Num s2 
    | Alnumdot s1 , Alnumdot s2
    | Email s1    , Email s2 
    | Pararef (_, s1), Pararef (_, s2) -> 
      if lowercase_conv then
        String.compare (Otext.lower s1) (Otext.lower s2)
      else
        String.compare s1 s2
    (*Combinations of types that were not listed are compared by typerank*)
    | _ -> compare_typerank t1 t2

end

(** Template module for using TokenSet's - use first class modules if
    wanting to control lowercase-conversion.*)
module TokenSet = Set.Make (struct 

  type t = TokenCmpStrict.t
  let compare = TokenCmpStrict.compare ~lowercase_conv:true (*~verbose:false *)
    
end)


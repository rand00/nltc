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

(* earlier experiment with new match types for matching sentences as well
type 'a match_ = {
  id0 : 'a;
  id1 : 'a;
  match_pct : float;
}

and token_id = int (*token id*) * int (*sentence id*) 
and token_match = token_id match_

and sentence_id = int
and sentence_match = sentence_id match_

and result_parts = 
  | Tokens of token_match list
  | Sentences of (sentence_match * token_match list) list

and result = {
  match_pct : float;
  matches : result_parts;
}
*)

let rec find f = function
  | [] -> None
  | e::tl -> 
    let succes, score = f e in
    if succes then Some (e, score) else 
      find f tl

(**Compares two texts with the laissez-faire method possible with the
   [Cmp_token.TokenCmp.equal] function. This function takes O(n*n) 
   worst case time, as each token from the first text is compared with the
   respectively compatible tokens from the other text (some token-type
   combinations are excluded).*)
(*goto
  . group alike tokens from each text 
    . and only compare one of these with all groups of other text?
      > bad thing about this is that we might loose matches; maybe we
        go through all tokens in a group and test if some token matches
        in other group; if yes, we quit comparison with success
*)
let cmp_loose ~equal_loose ~text_to_tokenwraps ~text_id tx1 tx2 = 
  let tx1_toks = text_to_tokenwraps tx1 
  and tx2_toks = text_to_tokenwraps tx2
  in
  let matches, acc_score = 
    List.fold_right 
      (fun tx1_tok ((matches, acc_score) as acc) -> 
         match 
           find (equal_loose tx1_tok) tx2_toks
         with
         | Some (tx2_tok, score) -> 
           ((tx1_tok, tx2_tok, score) :: matches
           , acc_score +. score)
         | None -> acc
      ) tx1_toks ([], 0.)
  in
  (text_id tx1, text_id tx2, acc_score), matches


(*goto return new result type*)
(*goto rewrite to fit new DB module structure*)
(**Same as [cmp_loose] but uses Set's for comparing sections; tokens need be 
   ordered (which cmp_loose doesn't need)*)
(*
let cmp_strict
    ~(callback_mod : (module CB.S))
    ~(tokenset_mod : (module Set.S with type elt = Token.token))
    tx1 tx2 = 
  let module TSet = (val tokenset_mod) in
  let module CB_arg = (val callback_mod) 
  in
  (*goto - if setting:bound_sntc = false => List.flatten*)
  (*> we don't use sentence bounds information for anything yet*)
  (*>goto for new structure - flatten + map to token-wrap (possibly with location information)
     > therefore TSet equality function also need to change
  *)
  let tokenize_flat tx = List.flatten (Tokenizer.of_string tx) in
  let set_of_list =
    let open Token in
    List.fold_left (fun acc token -> 
        TSet.add token acc
      ) TSet.empty 
  in
  let tx1_tokens = [set_of_list (tokenize_flat tx1.text)]
  and tx2_tokens = [set_of_list (tokenize_flat tx2.text)]
  in
  let comparisons =
    Combine.all2i TSet.inter tx1_tokens tx2_tokens
  in  
  (tx1.id, tx2.id),
  List.fold_right (fun (i1, i2s) acc1 -> 
      (List.fold_right (fun (i2, tset) acc2 -> 
           let tokens = TSet.elements tset in
           let len = List.length tokens in
           (*> Insertion of comparison stats into DB*)
           Lwt.async 
             (fun () -> CB_arg.put_mtokens tx1.id tx2.id ~len ~tokens);
           len + acc2
           (*goto > we should return the following instead:
             . tx.id's 
             . a map of token-matches (tuples) and their match-score
             . the accumulated score (the text-match-score)
           *)
         ) i2s 0) + acc1
    ) comparisons 0
*)

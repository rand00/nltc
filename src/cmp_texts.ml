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

(*goto define type + std-settings for comparisons of texts*)

(*goto place in separate module?*)
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
(*goto supply func-arg for 
    . wrapping tokens after tokenization / or just tokenize func-arg
      > type token.t list list -> token-wrap list
      > This also annihilates list.flatten from this code
    ? > 
      . get token from wrap / or just equal_score func-arg
      . get text from text-wrap? (or just tokenize-wrapper with text-type as arg)
      . how to get text id? <- get_txtID ? 
    . the last 3(?4) could be solved by a fc-module with func's for doing this
      > equal_score could also be defined in fc-mod?
    @ goto research where I would wan't to define these things 
      -> where should control be
    @ goto research future dependencies!!
      > equal_score would like to depend on token-wrap; not just token
        > this means that equal score could just as well be defined inside fc-module, 
          as it will depend on the type inside this (could of course get fun-args for unwrapping..)
          > useful now? NO - let's fuckin parametrize the world + all it's constituent atoms
  > see experi../loca..ml*)
(*goto goo call this func with correct args*)
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


(*for debugging parajobs*)
(*
let cmp_loose' ~equal_score (tx1_id, tx1_toks) (tx2_id, tx2_toks) = 
  let matches, acc_score = 
    List.fold_right 
      (fun tx1_tok ((matches, acc_score) as acc) -> 
         match 
           find 
             (fun tx2_tok -> equal_score tx1_tok tx2_tok) 
             tx2_toks
         with
         | Some (tx2_tok, score) -> 
           ((tx1_tok, tx2_tok, score) :: matches
           , acc_score +. score)
         | None -> acc
      ) tx1_toks ([], 0.)
  in
  (tx1_id, tx2_id, acc_score), matches
*)  

(*goto return new result type*)
(*goto rewrite to fit new DB module structure*)
(**Same as [cmp_twotexts_exp] but uses Set's for comparing sections*)
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

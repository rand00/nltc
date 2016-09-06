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


(**The [Analysis.run] function sets the analysis up with the right
   settings avoiding mutable state, and including static typing. It
   runs an analysis over the list of input-text's, emitting the words
   of the sentences that match to the [callback] function. *)
let run 
    ?(anl_sett=Settings.std_analysis)
    ~equal_loose
    ~text_to_tokenwraps
    ~text_id
    ~callback_mod
    (*<goto add cb for progress (in future separately for tokenization/comparison!)*)
    texts
  = 
  let open Settings in

  (**The loose comparison of words, partly based on the
     hamming-distance, paired with a collection of weights and other
     parameters.*)
  let run_loose () = 
    (*>goto this need to depend on tokenwrap type; so need to parametrize either
      TokenWrap or equal_loose for run-time configuration (TokenWrap fc-mod can be
      constructed before given to Analysis.run)*)
    let open Lwt in
    let save_free_cores = 2 in (*goto make argument to CLI*)
    let cores = max 1 (Parallel_jobs.cores () - save_free_cores) in
    let combinations = Combine.all (fun x y -> x,y) texts in
    let times_return = 4 in (*goto test different magnitudes - 
                              also using CB for progress info! (both CLI and DB insert)*)
    (*goto add job-queue to tokenize all texts to be compared (take out from cmp_loose); 
      then we don't need n/2 x tokenize*)
    let combs_chunked = 
      Parallel_jobs.chunk ~n:(cores*times_return) combinations in
    let jobs_cmp = 
      List.map (fun chunk () -> 
          Lwt_list.map_s (fun (x,y) ->
              (*goto goo give all args *)
              Cmp_texts.cmp_loose x y
                ~equal_loose
                ~text_to_tokenwraps
                ~text_id
              |> Lwt.return
            ) chunk
        ) combs_chunked 
    in
    Parallel_jobs.Naive.exec jobs_cmp ~force_cores:(Some cores)
    >|= List.flatten 

  (**This is (should be) the faster analysis compared with the 'loose' analysis*)
(*
  and run_strict () =
    let tokenset_mod = 
      let module TS = Set.Make(struct
        open Cmp_token
        include TokenCmpStrict
        let compare = compare 
          ~lowercase_conv:cmp_sett.word_sett.lowercase_convert
      end) 
      in 
      (module TS : Set.S 
        with type elt = Cmp_token.TokenCmpStrict.t) 
    in
    Combine.all 
      (Cmp_texts.cmp_strict
         ~callback_mod
         ~tokenset_mod)
      texts
*)
  in 
  match anl_sett with 
  | {mode = `Strict} -> Lwt.fail_with "Strict comparison deprecated"
  (*Lwt.return (run_strict ())*)
  | {mode = `Loose}  -> run_loose ()


(*goto > remove this when not testing anymore*)
(*
let _ = begin
  print_any stdout (run ());
  print_endline ""
end
*)

let compare_txtmatch_on_score ((_, _, txm_score), _) ((_, _, txm_score'), _) =
  Float.compare txm_score' txm_score



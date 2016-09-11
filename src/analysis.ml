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

module PJobs = Parallel_jobs

let run 
    ?(anl_sett=Settings.std_analysis)
    ~equal_loose
    ~text_to_tokenwraps
    ~text_id
    ~callback_mod
    (*<goto add cb for progress (in future separately for tokenization/comparison!)*)
    texts
  = 
  let open Settings in (*goto think how to design/place this module, 
                         kind of deprecated*)
  let run_loose () = 
    let open Lwt in
    let save_free_cores = 2 in (*goto make argument to CLI*)
    let cores = max 1 (PJobs.cores () - save_free_cores) in
    let times_return = 4 in
    (*<goto 
      . test different magnitudes - also using CB for progress info! (both CLI and DB insert)
        < brian over how to consistently summarize progress instead of relying on DB atomicity
      . make times_return CLI arg
    *)
    texts
    |> PJobs.chunk ~n:(cores*times_return) 
    |> PJobs.of_chunks (fun tx ->
        (text_id tx, text_to_tokenwraps tx) |> Lwt.return
      )
    |> PJobs.Naive.exec ~force_cores:(Some cores)
    >|= List.flatten
    >>= fun tokenwraps -> (
      tokenwraps
      |> Combine.all (fun x y -> x,y) 
      |> PJobs.chunk ~n:(cores*times_return) 
      |> PJobs.of_chunks (fun (x,y) ->
          Cmp_texts.cmp_loose x y ~equal_loose |> Lwt.return
        ) 
      |> PJobs.Naive.exec ~force_cores:(Some cores)
    )
    >|= List.flatten

  (**This is (should be) the faster analysis compared with the 'loose' analysis,
     but depends on an ordered comparison*)
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

let compare_txtmatch_on_score ((_, _, txm_score), _) ((_, _, txm_score'), _) =
  Float.compare txm_score' txm_score



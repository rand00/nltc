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
open Lwt


let chunk ~n l = 
  let arr = Array.of_list l in
  let len_arr = Array.length arr in
  let size_chunk = 
    max 1
      (Int.of_float @@ 
       Float.round (float len_arr /. float n)) in
  let ret = Array.make n [] in
  for i = 0 to pred len_arr do
    let chunk_pos = min (pred n) (i / size_chunk) in 
    ret.(chunk_pos) <- arr.(i) :: ret.(chunk_pos);
  done;
  Array.to_list ret

let force_cores_default = None
let fallback_cores_default = 1

let cores 
    ?(fallback_cores=fallback_cores_default) 
    ?(force_cores=force_cores_default) 
    () = 
  match force_cores with 
  | Some cores -> cores
  | None -> 
    begin match 
        Unix.run_and_read 
          "grep processor /proc/cpuinfo | wc -l"
      with 
      | Unix.WEXITED 0, cores -> 
        String.strip cores |> Int.of_string
      | _ -> fallback_cores
    end


module Naive = struct 

  let exec_aux 
      ?(fallback_cores=fallback_cores_default) 
      ?(force_cores=force_cores_default) 
      jobs = 
    let ncores = cores ~fallback_cores ~force_cores () in
    let take_job = 
      let guard = Lwt_mutex.create () in
      fun () -> 
        Lwt_mutex.with_lock guard 
          (fun () -> Lwt_stream.get jobs)
    in
    let results, push_result = 
      let results, push_result = Lwt_stream.create () in
      let guard = Lwt_mutex.create () in
      let push r = 
        Lwt_mutex.with_lock guard
          (fun () -> push_result @@ r; Lwt.return ())
      in results, push
    in
    let rec worker () = 
      take_job () >>= function
      | Some job -> 
        Parallel.run job () 
        >>= (push_result%Option.some)
        >> worker ()
      | None -> Lwt.return ()
    in
    let rec workers ncores = 
      let rec aux acc = function
        | 0 -> acc
        | n -> aux ((worker ())::acc) (pred n) in
      aux [] ncores
    in
    Lwt.join @@ workers ncores
    >> push_result None
    >> Lwt_stream.to_list results

  let exec
      ?(fallback_cores=fallback_cores_default) 
      ?(force_cores=force_cores_default) 
      jobs = 
    Lwt_stream.of_list jobs
    |> exec_aux ~fallback_cores ~force_cores

  let exec_arr
      ?(fallback_cores=fallback_cores_default) 
      ?(force_cores=force_cores_default) 
      jobs = 
    Lwt_stream.of_array jobs
    |> exec_aux ~fallback_cores ~force_cores



end 



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

module Sql = Sqlexpr_sqlite_lwt

module type S = sig 

  val put_mtokens : 
    tokens : Token.t list -> 
    tid1 : 'a -> 
    tid2 : 'a -> 
    unit Lwt.t

end

module NoAction : S = struct 

  let put_mtokens ~tokens ~tid1 ~tid2 = Lwt.return ()

end

module Print : S = struct

  open Lwt
  
  let put_mtokens ~tokens ~tid1 ~tid2 = 
    Lwt_list.iter_s (fun t -> 
        Lwt_io.printf "   %s\n" (Token.to_tstring t))
      tokens

end


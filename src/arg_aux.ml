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

module Sqlexpr = Sqlexpr_sqlite_lwt
(*
module Sqex = Sqlexpr (*module Sqlexpr need to be here for syntax extension*)
*)
    
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

let id x = x

(* > goto fix for new code + when implemented PompV2 db actions

let run_anal_pomp ?(limit=None) ?(section_id=19) ~db ~anal_id = 
  match db with 
  | db, `V1 -> 
    let callback_mod = 
      (module struct
        let put_mtokens = (DB.PompV1.Ins.stats_v1 ~db ~anal_id)
      end : CB.IntfA) in
    DB.PompV1.Init.stats db >>
    DB.PompV1.Sel.section_concat ~section_id db
    (* < goto goo <- we need to read anal-settings from a table and use 'sections'*)
    >|= Analysis.run ~callback_mod
    (*< goto we need to control which algo to use; cmp_sett, anl_sett (use beforementioned)*)
    >>= fun _ -> Lwt.return_unit
   (* < goto goo <- iter over result of analysis and insert in db instead of incremental update*)
   (* < goto later <- make cb-mod update progress once in a while (not each time) 
     (do this by having internal state or given 'i' as arg?)*)
  | db, `V2 -> 
    assert false
    (*< goto implement v2 version when DB.V2 is implemented*)
*)
  
module Headers = struct 

  let gen_headers name arg = [
    "token",       "\nCmdline "^name^": Tokens:";
    "token_types", "\nCmdline "^name^": Tokens with types:";
    "content",     "\nCmdline "^name^": Content:";
    "compare",     "\nCmdline "^name^": Compare:";
    "_unknown_",   "Option given to arg "^arg^" unknown.";
  ]

  let local_db = gen_headers "local-db" "--db-local"
  let pomp_db = gen_headers "pomp-db" "--db-pomp"


  let header_of_arg headers cli_arg = 
    try
      List.assoc cli_arg headers
    with Not_found ->
      List.assoc "_unknown_" headers 

  
end

let sentence_sep = 
  "\n\n--------------- \
   NEW SENTENCE \
   ----------------\n"

open Text_entry

let print_doc_id ?i id =
  match id,i with 
  | `Id id, _ -> Lwt_io.printf "\nDocument %d:\n" id
  | `Name name, Some i -> Lwt_io.printf "\nDocument %d --- \
                                         Text-name: %s\n" i name
  | `Name name, None -> Lwt_io.printf "\nText-name: %s\n" name

(*goto pmatch on text-types here as well*)
let print_content texts =
  Lwt_list.iteri_s 
    (fun i {id; text} ->
       print_doc_id ~i id >> Lwt_io.printf "  %s" text)
    texts
  >> Lwt_io.printl ""

(*goto goo pmatch on text-types 
  . (make new sumtype containing diff locs + diff text catted structure)*)
let tokenize_print texts header token_to_str = 
  Lwt_io.printl header >> 
  texts >>= Lwt_list.iter_s
    (fun {id; text} -> 
       print_doc_id id >> 
       (text 
        |> Otext.to_stream 
        |> Tokenizer.of_stream 
        |> Tokenizer.print_lwt sentence_sep token_to_str
        >> Lwt_io.printl ""))
  >> Lwt_io.printl 
    "------------ Tokenize-print DONE --------------"

let pomp_sections arg = 
  let rec aux acc = function 
    | "all" -> Some `All
    | <:re< [",;"]* (["0"-"9"]+ as id) (_* as rest) >> -> 
      ( match acc with 
        | Some ids -> aux (Some (int_of_string id :: ids)) rest
        | None ->     aux (Some [int_of_string id]) rest )
    | s -> 
      ( match acc with 
        | Some ids -> Some (`List ids)
        | None -> None)
  in aux None arg

let compare_score ((_, _, txm_score), _) ((_, _, txm_score'), _) =
  Float.compare txm_score' txm_score

let print_analysis_results txt_matches ~show_token ~show_txtID =
  let print_tok_match (t,t',score) =
    Lwt_io.printf "%30s matches %s with score %f\n"
      (*>gomaybe this func also need match token subvariants (for loc)*)
      (show_token t)
      (show_token t')
      score in
  let print_txt_match ((tx1id, tx2id, txm_score), matches) = 
    Lwt_io.printf "Document \"%s\" and Document \"%s\"\
                   , match-score -->  %f\n"
      (show_txtID tx1id) 
      (show_txtID tx2id) 
      txm_score
    >> Lwt_list.iter_s print_tok_match matches
  in
  Lwt_list.iter_s print_txt_match txt_matches
  >> Lwt_io.printl ""  

module Handler = struct 

  (*>goto - do we even need these witnesses as we can just keep types abstract 
    by implementing all needed functions in interface.. *)
  (*This makes us able to make fc-mod's types un-abstract for return-values*)
  type _ db_witness =
    | LocalDB :
        ( DB.Local.T.text_id * 
          DB.Local.T.token_wrap *
          DB.Local.T.score
        ) db_witness
    | PompDB_V1 :
        ( DB.PompV1.T.text_id *
          DB.PompV1.T.token_wrap *
          DB.PompV1.T.score
        ) db_witness
    | PompDB_V2 :
        ( DB.PompV2.T.text_id *
          DB.PompV2.T.token_wrap *
          DB.PompV2.T.score
        ) db_witness
          

  (*goto modify func's to fit new module structure*)
  let localDB db cli_arg =
    let witness = LocalDB in
    (*goto
      > the following seems pretty reuseable; seems like it would be nice to 
        make this function polymorphic using fc-modules 
        > then we can later find a solution for non-matching args (extension of functionality)
        > the only major difference between db's are e.g. the filters - 
          just apply these (by extracting texts) before giving them to this function
      . make modules 
        . and use them for printing tokenwraps (with new arg-type) (not for localDB)
        . and for printing text content 
        . for str_of_text-id to print
        . and for tokenizing 
        . and for comparing score 
      . pass these modules on to analysis 
    *)
    let texts = DB.Local.Sel.texts db in
    let printheader = Headers.header_of_arg Headers.pomp_db cli_arg 
    in match cli_arg with 
    | "token" ->
      tokenize_print texts printheader Token.to_string
    | "token_types" ->
      tokenize_print texts printheader Token.to_tstring
    | "content" ->
      Lwt_io.printl printheader >> print_content texts
    | "compare" ->
      begin
        Lwt_io.printl printheader >> 
        texts >>= Analysis.run ~callback_mod 
        >|= List.sort compare_score
        >>= print_analysis_results
          ~show_token:Token.to_tstring
          ~show_txtID:id
      end
    | _ -> (prerr_endline printheader; exit 1)
  
  
  let pompDB_v1 db ?(sections=`All) cli_arg =
    let texts = DB.PompV1.Sel.sections ~sections db in
    let printheader = Headers.header_of_arg Headers.pomp_db cli_arg 
    in match cli_arg with 
    | "token" ->
      tokenize_print texts printheader Token.to_string
    | "token_types" ->
      tokenize_print texts printheader Token.to_tstring
    | "content" ->
      Lwt_io.printl printheader >> print_content texts
    | "compare" ->
      begin
        Lwt_io.printl printheader >> 
        texts >>= Analysis.run ~callback_mod 
        >|= List.sort compare_score
        >>= print_analysis_results
      end
    | _ -> (prerr_endline printheader; exit 1)


  let pompDB_v2 db
      ?(filters=DB.PompV2.T.({docs=`All; sects=`All}))
      cli_arg =
    (*>goto give correct filter-args*)
    let texts = DB.PompV2.Sel.sections ~sections db in
    let printheader = Headers.header_of_arg Headers.pomp_db cli_arg 
    (*>goto supply/map correct functions*)
    in match cli_arg with 
    | "token" ->
      tokenize_print texts printheader Token.to_string
    | "token_types" ->
      tokenize_print texts printheader Token.to_tstring
    | "content" ->
      Lwt_io.printl printheader >> print_content texts
    | "compare" ->
      begin
        Lwt_io.printl printheader >> 
        texts >>= Analysis.run ~callback_mod 
        >|= List.sort compare_score
        >>= print_analysis_results
      end
    | _ -> (prerr_endline printheader; exit 1)

  
end


let run_db_cli : 'db -> 'arg -> unit Lwt.t 
  = fun db cli_arg -> match db with 
    | `Local db ->
      Handler.localDB db cli_arg
    | `Pomp_v1 (db, sections) ->
      Handler.pompDB_v1 db cli_arg ~sections
    | `Pomp_v2 (db, filters) ->
      Handler.pompDB_v2 db cli_arg ~filters


(*> goto remove when refactored 

let run_db_cli db cli_arg = match db with 
  | `Local db -> 
    arghandler cli_arg
      ~callback_mod:(module Cb.NoAction : Cb.IntfA) (*(module Cb.Print : Cb.IntfA) *)
      ~headers:Headers.local_db 
      ~texts:(DB.L.Sel.texts db)
  (*<goto need be a new sum-type mapping over different text structures, 
    as we need location info now and don't want to implement everything*)       
  | `Pomp (db, `V1, sections) -> 
    arghandler cli_arg
      ~callback_mod:(module Cb.NoAction : Cb.IntfA) 
      ~headers:Headers.pomp_db 
      ~texts:(DB.P.V1.Sel.sections ~sections db) 
  | `Pomp (db, `V2, sections) -> 
    arghandler cli_arg
      ~callback_mod:(module Cb.NoAction : Cb.IntfA) 
      ~headers:Headers.pomp_db 
      ~texts:(DB.P.V2.Sel.sections ~sections db) 

*)





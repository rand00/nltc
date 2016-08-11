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

(*goto 
  . take tokenwraps + show-id + show-content  as arg? or just put into common-handler
  . also take show_tokenwrap as arg (this can be composed in handler for choosing text-repr)
*)
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


(*goto
    . make modules 
      . and use them for printing tokenwraps (with new arg-type) (not for localDB)
      . and for printing text content 
      . for str_of_text-id to print
      . and for tokenizing 
      . and for comparing score 
    . pass these modules on to analysis 
  *)
let common_handler
    ~db
    ~cli_arg
    ~texts
    ~header
    ~textentry_mod
    ~tokenwrap_mod
    ~eq_tokenwrap_mod
    ~callback_mod
  = 
  match cli_arg with 
  | "token" ->
    tokenize_print texts header Token.to_string
  | "token_types" ->
    tokenize_print texts header Token.to_tstring
  | "content" ->
    Lwt_io.printl header >> print_content texts
  | "compare" ->
    begin
      Lwt_io.printl header >> 
      texts >>= Analysis.run ~callback_mod 
      >|= List.sort Analysis.compare_tmatch_on_score
      >>= print_analysis_results
        ~show_token:Token.to_tstring
        ~show_txtID:id
    end
  | _ -> (prerr_endline header; exit 1)
  

(*>We grab texts first to support different function interfaces over time*)
(*goto goo
  . give db fc-mods as arg
  . callback-mod can be chosen/built dynamically from cli-arg spec
*)
let run_db_cli : 'db -> 'arg -> unit Lwt.t 
  = fun db cli_arg -> match db with 
    | `Local db ->
      common_handler ~db ~cli_arg
        ~texts:(DB.Local.Sel.texts db)
        ~header:(Headers.header_of_arg Headers.local_db cli_arg)
        ~textentry_mod:(module DB.Local.TextEntry : DB.TEXTENTRY)
        ~tokenwrap_mod:(module DB.Local.TokenWrap_Naked : DB.TOKENWRAP)
        ~eq_tokenwrap_mod:(module DB.Local.Eq_TokenWrap : DB.EQ_TOKENWRAP)
        ~callback_mod:(module CB.NoAction : CB.S)

    | `Pomp_v1 (db, sections) -> 
      common_handler ~db ~cli_arg
        ~texts:(DB.PompV1.Sel.texts ~sections db)
        ~header:(Headers.header_of_arg Headers.pomp_db cli_arg)
        ~textentry_mod:(module DB.PompV1.TextEntry : DB.TEXTENTRY)
        ~tokenwrap_mod:(module DB.PompV1.TokenWrap_Naked : DB.TOKENWRAP)
        ~eq_tokenwrap_mod:(module DB.PompV1.Eq_TokenWrap : DB.EQ_TOKENWRAP)
        ~callback_mod:(module CB.NoAction : CB.S)

    | `Pomp_v2 (db, filters) ->
      common_handler ~db ~cli_arg
        ~texts:(DB.PompV2.Sel.texts ~filters db)
        ~header:(Headers.header_of_arg Headers.pomp_db cli_arg)
        ~textentry_mod:(module DB.PompV2.TextEntry : DB.TEXTENTRY)
        ~tokenwrap_mod:(module DB.PompV2.TokenWrap : DB.TOKENWRAP)
        ~eq_tokenwrap_mod:(module DB.PompV2.Eq_TokenWrap : DB.EQ_TOKENWRAP)
        ~callback_mod:(module CB.NoAction : CB.S)






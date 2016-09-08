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

type options = {
  txtmatch_lowbound : float option
}

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


let print_text_id id_str =
  Lwt_io.printf "\n\nDocument %s:\n" id_str

let print_content ~show_tid ~show_text texts =
  Lwt_list.iter_s (fun text ->
      print_text_id (show_tid text)
      >> Lwt_io.printf "  %s" (show_text text)
    ) texts
  >> Lwt_io.printl ""

let sentence_sep = 
  "\n\n"^
  "--------------------- NEW SENTENCE ---------------------"
  ^"\n"

let parse_filter arg = 
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
      (*>can be used by caller for printing loc info too*)
      (show_token t)
      (show_token t')
      score in
  let print_txt_match ((tx1id, tx2id, txm_score), matches) = 
    Lwt_io.printf "\nDocument \"%s\" and Document \"%s\"\
                   , match-score -->  %f\n"
      (show_txtID tx1id) 
      (show_txtID tx2id) 
      txm_score
    >> Lwt_list.iter_s print_tok_match matches
  in
  Lwt_list.iter_s print_txt_match txt_matches
  >> Lwt_io.printl ""  

let filter_txtmatches_on ?(txtmatch_lowbound=None) txtmatches =
  match txtmatch_lowbound with
  | Some txtmatch_lowbound -> 
    List.filter (fun ((_, _, txm_score), _) ->
        txm_score >= txtmatch_lowbound
      ) txtmatches
  | None -> txtmatches

(*gomaybe move the signature into mli instead, if possible?*)
let common_handler :
  type text_entry token_wrap . 
  cli_arg:'a -> 
  texts:text_entry list Lwt.t ->
  header:'a ->
  textentry_mod:
  (module DB.TEXTENTRY
    with type t = text_entry) ->
  tokenwrap_mod:
  (module DB.TOKENWRAP
    with type t = token_wrap 
     and type text_entry = text_entry) -> 
  eq_tokenwrap_mod:
  (module DB.EQ_TOKENWRAP
    with type t = token_wrap
     and type score = float) -> 
  callback_mod:(module CB.S) ->
  options:options -> 
  unit Lwt.t
  = fun 
    ~cli_arg
    ~texts
    ~header
    ~textentry_mod
    ~tokenwrap_mod
    ~eq_tokenwrap_mod
    ~callback_mod
    ~options
    -> 
      let module TextEntry = (val textentry_mod) in
      let module TokenWrap = (val tokenwrap_mod) in
      let module Eq_TokenWrap = (val eq_tokenwrap_mod)
      in
      match cli_arg with 

      | "token" ->
        Lwt_io.printl header >>
        (texts >>=
         Lwt_list.iter_s
           (fun text ->
              Lwt_io.printl (text |> TextEntry.(show_id%id)) >>
              (TokenWrap.text_to_wraps text
               |> TokenWrap.pp_wraps
                 ~print_location:true (*goto make depend on cli-arg (++following)*)
                 ~show_token:Token.to_string
                 ~sntc_sep:sentence_sep)
           )
         >> Lwt_io.printl 
           "\n--------------- Printing of tokens DONE ----------------"
        )

      | "token_types" ->
        Lwt_io.printl header >> 
        (texts >>=
         Lwt_list.iter_s
           (fun text ->
              Lwt_io.printl (text |> TextEntry.(show_id%id)) >>              
              (TokenWrap.text_to_wraps text
               |> TokenWrap.pp_wraps
                 ~print_location:true
                 ~show_token:Token.to_tstring
                 ~sntc_sep:sentence_sep)
           )
         >> Lwt_io.printl 
           "\n----------- Printing of tokens + types DONE ------------"
        )

      | "content" ->
        Lwt_io.print header >>
        texts >>=
        print_content 
          ~show_tid:TextEntry.(show_id%id)
          ~show_text:TextEntry.(show_text%text)
        >> Lwt_io.printl 
          "\n------------ Printing of text-content DONE --------------"

      | "compare" ->
        Lwt_io.printl header >>
        (texts 
         >>= Analysis.run
           ~text_to_tokenwraps:TokenWrap.text_to_wraps
           ~text_id:TextEntry.id
           ~equal_loose:Eq_TokenWrap.equal_loose
           ~callback_mod
         >|= filter_txtmatches_on
           ~txtmatch_lowbound:options.txtmatch_lowbound
         >|= List.sort Analysis.compare_txtmatch_on_score
         >>= print_analysis_results
           ~show_token:(Token.to_tstring%TokenWrap.token)
           (*< goto depend on printing of loc info cli-arg*)
           ~show_txtID:TextEntry.show_id
        )
        >> Lwt_io.printl 
          "-------------- Comparison of texts DONE ----------------"

      | _ -> (prerr_endline header; exit 1)
  

(*goto
  . callback-mod can be chosen/built dynamically from cli-arg spec
*)
(*>We grab texts first to support different function interfaces over time*)
let run_db_cli : db:'db -> options:options -> 'arg -> unit Lwt.t 
  = fun ~db ~options cli_arg -> match db with 
    | `Local db ->
      common_handler ~cli_arg
        ~texts:(DB.Local.Sel.texts db)
        ~header:(Headers.header_of_arg Headers.local_db cli_arg)
        ~textentry_mod:(module DB.Local.TextEntry)
        ~tokenwrap_mod:(module DB.Local.TokenWrap)
        ~eq_tokenwrap_mod:(module DB.Local.Eq_TokenWrap)
        ~callback_mod:(module CB.NoAction)
        ~options

    | `Pomp_v1 (db, sections) -> 
      common_handler ~cli_arg
        ~texts:(DB.PompV1.Sel.texts ~sections db)
        ~header:(Headers.header_of_arg Headers.pomp_db cli_arg)
        ~textentry_mod:(module DB.PompV1.TextEntry)
        ~tokenwrap_mod:(module DB.PompV1.TokenWrap)
        ~eq_tokenwrap_mod:(module DB.PompV1.Eq_TokenWrap)
        ~callback_mod:(module CB.NoAction)
        ~options

    | `Pomp_v2 (db, filters) ->
      common_handler ~cli_arg
        ~texts:(DB.PompV2.Sel.texts ~filters db)
        ~header:(Headers.header_of_arg Headers.pomp_db cli_arg)
        ~textentry_mod:(module DB.PompV2.TextEntry)
        ~tokenwrap_mod:(module DB.PompV2.TokenWrap)
        ~eq_tokenwrap_mod:(module DB.PompV2.Eq_TokenWrap)
        ~callback_mod:(module CB.NoAction)
        ~options





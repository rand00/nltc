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
let cli_handler :
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
         >|= snd
         >|= filter_txtmatches_on
           ~txtmatch_lowbound:options.txtmatch_lowbound
         >|= List.sort Analysis.compare_txtmatch_on_score
         >>= print_analysis_results
           ~show_token:(Token.to_tstring%TokenWrap.token)
           (*< goto depend on printing of loc info cli-arg*)
           ~show_txtID:TextEntry.show_id
        )
        >> Lwt_io.printl 
          "\n--------------- Comparison of texts DONE ----------------"

      | _ -> (prerr_endline header; exit 1)
  

(*goto
  . callback-mod can be chosen/built dynamically from cli-arg spec
*)
(*>We grab texts first to support different function interfaces over time*)
let run_db_cli : db:'db -> options:options -> 'arg -> unit Lwt.t 
  = fun ~db ~options cli_arg -> match db with 
    | `Local db ->
      cli_handler ~cli_arg
        ~texts:(DB.Local.Sel.texts db)
        ~header:(Headers.header_of_arg Headers.local_db cli_arg)
        ~textentry_mod:(module DB.Local.TextEntry)
        ~tokenwrap_mod:(module DB.Local.TokenWrap)
        ~eq_tokenwrap_mod:(module DB.Local.Eq_TokenWrap)
        ~callback_mod:(module CB.NoAction)
        ~options

    | `Pomp_v1 (db, sections) -> 
      cli_handler ~cli_arg
        ~texts:(DB.PompV1.Sel.texts ~sections db)
        ~header:(Headers.header_of_arg Headers.pomp_db cli_arg)
        ~textentry_mod:(module DB.PompV1.TextEntry)
        ~tokenwrap_mod:(module DB.PompV1.TokenWrap)
        ~eq_tokenwrap_mod:(module DB.PompV1.Eq_TokenWrap)
        ~callback_mod:(module CB.NoAction)
        ~options

    | `Pomp_v2 (db, filters) ->
      cli_handler ~cli_arg
        ~texts:(DB.PompV2.Sel.texts ~filters db)
        ~header:(Headers.header_of_arg Headers.pomp_db cli_arg)
        ~textentry_mod:(module DB.PompV2.TextEntry)
        ~tokenwrap_mod:(module DB.PompV2.TokenWrap)
        ~eq_tokenwrap_mod:(module DB.PompV2.Eq_TokenWrap)
        ~callback_mod:(module CB.NoAction)
        ~options


let run_analysis_pomp ~db ~an_id = 
  match db with 
  (* > gomaybe fix for new code + when implemented PompV2 db actions *)
  | db, `V1 ->
    Lwt.fail_with "Nltc: We do not support Pomp V1 for DB-based analysis for now."
    (*
    let callback_mod = 
      (module struct
        let put_mtokens = (DB.PompV1.Ins.stats_v1 ~db ~anal_id)
      end : CB.IntfA) in
    DB.PompV1.Init.stats db >>
    DB.PompV1.Sel.section_concat ~section_id db
    >|= Analysis.run ~callback_mod
    >>= fun _ -> Lwt.return_unit
    *)
  | db, `V2 -> 
    (*> goto implement v2 version when DB.V2 is implemented
      . (later) load settings from db (make easily read specification to andreas)
      . (later) check all db-tables for consistency + check that the data we need is there
      . DB: implement insertion procs
      . change 'run-analysis' job in Nltc 
    *)
    let open DB.PompV2
    in
    let cmp_token_settings = Cmp_token.std_cmp_settings
    in
    let equal_token_loose t t' =
      let token = TokenWrap.token in
      Cmp_token.TokenCmpLoose.equal (token t) (token t')
        ~settings:cmp_token_settings
        ~verbose:false
    in
    let txt_filters =
      { T.datasets = `All;
        T.docs = `All;
        T.sects = `All; }
    and options = { txtmatch_lowbound = None } in
    let callback_mod = (module CB.NoAction : CB.S) 
    in
    Sel.texts ~filters:txt_filters db
    >>= Analysis.run
      ~text_to_tokenwraps:TokenWrap.text_to_wraps
      ~text_id:TextEntry.id
      ~equal_loose:equal_token_loose
      ~callback_mod
    >>= fun (tokenwraps,txtmatches) ->
    Ins.clear db [`TokenWraps; `TxtMatches] >>
    Ins.tokenwraps tokenwraps
    >>= fun tokenwrap_ids -> 
    (txtmatches 
     |> filter_txtmatches_on
       ~txtmatch_lowbound:options.txtmatch_lowbound
     |> Ins.txtmatches tokenwrap_ids)

(*goo*)
    (*<goto insert tokens and txtmatches in db (while keeping track of 
      sntc-id's, token-id's etc. for inserting next depending elem)
      . for now we clear table before insertion (later we can memoize tokens)
      . need implement 'Ins.tokenwraps' that returns Map of tokenwrap -> id (int)
        . internally we keep map of [ sntc-pos (part-id/sntc-id) -> sntc-id ] 
      . need implement 'Ins.txtmatches' that takes map as arg 
    *)

  



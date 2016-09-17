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
module Sqex = Sqlexpr
let (>>=) = Lwt.(>>=)
let (>|=) = Lwt.(>|=)

module type TEXTENTRY =
sig
  type id
  (** id + also possibly contains extra info such as 'parts' 
      and 'sections' *)
  type t
  type content

  val id : t -> id
  val text : t -> content (*goto remove? .. seems useless on polymorphic code*)

  val show_id : id -> string
  val show_text : content -> string

end

(*>goto 
  . should be extended with strict comparison func. 
    (and other algo's like hamming++ later)
  . we might want score-compare, as score can be a type not suited for comparison by default 
*)
module type EQ_TOKENWRAP =
sig
  type t
  type score (*e.g. float or something more informative*)

  val equal_loose : t -> t -> bool * score
  val compare_score : score -> score -> int
  val show_score : score -> string
end

module type TOKENWRAP = (*functor (Eq:EQ) ->*)
sig
  type t
  type location
  type location_info
  
  val token : t -> Token.t
  val location : t -> location 
  val wraps_of_sntcs : location_info -> Token.t list list -> t list
  val pp_wrap :
    t -> 
    print_location:bool ->
    show_token:(Token.t -> string) ->
    unit Lwt.t
  val pp_wraps :
    ?print_location:bool ->
    show_token:(Token.t -> string) ->
    sntc_sep:string ->
    t list ->
    unit Lwt.t

  type text_entry
  val text_to_wraps : text_entry -> t list
  (*<gomaybe: later: can be parametrized with tokenizer if we construct 
    diff. versions*)
end


(** Local db for importing text through cmdline interface*)
module Local = struct

  module T = struct 

    type text_id = string
    type text_content = string
    type text_entry = {
      filename : text_id; 
      text : text_content;
    }

    type token_location = {
      token_id : int;
      sntc_id : int;
    }

    type token_wrap = {
      token : Token.t;
      location : token_location;
    }

    type score = float

  end
  open T

  module TextEntry :
    (TEXTENTRY
     with type id = T.text_id
      and type t = T.text_entry
      and type content = T.text_content) =
  struct

    type id = T.text_id
    type t = T.text_entry
    type content = T.text_content
    
    let id {filename} = filename
    let show_id filename = filename
    
    let text {text} = text
    let show_text text = text

  end

  module TokenWrap :
    (TOKENWRAP
     with type t = T.token_wrap 
      and type text_entry = T.text_entry
      and type location = T.token_location) =
  struct

    type t = T.token_wrap
    type location = T.token_location
    type location_info = unit

    let token {token} = token
    let location {location} = location
    let wraps_of_sntcs () sentences =
      let sentence_to_wraps sntc_id =
        List.mapi (fun token_id token ->
            { token;
              location = { token_id; sntc_id }
            }
          )
      in
      sentences      
      |> List.mapi sentence_to_wraps 
      |> List.flatten

    type text_entry = T.text_entry
    let text_to_wraps {text} = 
      Tokenizer.of_string text
      |> wraps_of_sntcs ()

    let pp_wrap ({token; location={token_id; sntc_id}}) ~print_location ~show_token =
      match print_location with
      | false -> Lwt_io.printf "%s " (show_token token)
      | true -> begin
          Lwt_io.printf
            " { token = \"%s\"; token_id = %d; sentence_id = %d }\n"
            (show_token token)
            token_id
            sntc_id
        end

    let pp_wraps ?(print_location=false) ~show_token ~sntc_sep token_wraps =
      let last_sntc = ref None in
      Lwt_list.iter_s (fun tw ->
          match !last_sntc with
          | None -> pp_wrap ~print_location ~show_token tw
          | Some last_sntc_id ->
            if tw.location.sntc_id <> last_sntc_id then begin
              last_sntc := Some tw.location.sntc_id;
              Lwt_io.printl sntc_sep
              >> pp_wrap ~print_location ~show_token tw
            end else
              pp_wrap ~print_location ~show_token tw
        ) token_wraps
    
  end

  (*Std. version of this module - should instead be defined from CLI-args*)
  module Eq_TokenWrap :
    (EQ_TOKENWRAP with type t = T.token_wrap
                   and type score = T.score) =
  struct

    type t = T.token_wrap
    type score = T.score

    let equal_loose t t' =
      let token = TokenWrap.token in
      Cmp_token.TokenCmpLoose.equal
        (*goto> these args should be controlled from CLI*)
        ~verbose:false
        ~settings:Cmp_token.std_cmp_settings
        (token t) (token t')

    let compare_score = Float.compare
    let show_score = Float.to_string 
    
  end
  
  let init_tables db =
    Sqex.execute db
      [%sqlinit "CREATE TABLE IF NOT EXISTS documents (
                   id INTEGER PRIMARY KEY,
                   text TEXT,
                   filename TEXT UNIQUE
                 );"]

  let del_docs db = 
    Sqex.execute db [%sql "DELETE FROM documents" ]

  module Ins = struct 

    (** Overwrites text that comes from files of the same name *)
    let replace_filetext =
      [%sqlc "INSERT OR REPLACE INTO documents(filename, text) VALUES(%s, %s)" ]

  end

  module Sel = struct 

    let texts db = 
      Sqex.select db [%sqlc "SELECT @s{filename},@s{text} FROM documents" ]
      >>= Lwt_list.map_s
        (fun (filename,text) -> Lwt.return {filename; text})

  end

end


(**Pomp system db; integrated with java + php*)
module PompV1 = struct 

  module T = struct

    type text_content = string
    type text_id = int
    type text_entry = {
      id : text_id;
      text : text_content;
    }
    type token_location = {
      token_id : int;
      sntc_id : int;
    }

    type token_wrap = {
      token : Token.t;
      location : token_location;
    }

    type score = float

    (*howto : how to use this type for specification, 
      while it doesn't need to be part of interface
      > make specific type declarations in Arg_aux module*)
    type filter_sections = [
      | `Sections of int list
      | `All
    ]

  end
  open T

  module TextEntry :
    (TEXTENTRY
     with type id = T.text_id
      and type t = T.text_entry) =
  struct

    type id = T.text_id
    type content = T.text_content
    type t = T.text_entry
    
    let id {id} = id
    let show_id = Int.to_string

    let text {text} = text
    let show_text text = text
    
  end

  module TokenWrap :
    (TOKENWRAP with type t = T.token_wrap
                and type text_entry = T.text_entry
                and type location = T.token_location) =
  struct
    type t = T.token_wrap
    type location = T.token_location
    type location_info = unit

    let token {token} = token
    let location {location} = location
    let wraps_of_sntcs () sentences =
      let sentence_to_wraps sntc_id =
        List.mapi (fun token_id token ->
            { token;
              location = { token_id; sntc_id }
            }
          )
      in
      sentences      
      |> List.mapi sentence_to_wraps 
      |> List.flatten

    type text_entry = T.text_entry
    let text_to_wraps {text} = 
      Tokenizer.of_string text
      |> wraps_of_sntcs ()

    let pp_wrap ({token; location={token_id; sntc_id}}) ~print_location ~show_token =
      match print_location with
      | false -> Lwt_io.printf "%s " (show_token token)
      | true -> begin
          Lwt_io.printf
            " { token = \"%s\"; token_id = %d; sentence_id = %d }\n"
            (show_token token)
            token_id
            sntc_id
        end

    let pp_wraps ?(print_location=false) ~show_token ~sntc_sep token_wraps =
      let last_sntc = ref None in
      Lwt_list.iter_s (fun tw ->
          match !last_sntc with
          | None -> pp_wrap ~print_location ~show_token tw
          | Some last_sntc_id ->
            if tw.location.sntc_id <> last_sntc_id then begin
              last_sntc := Some tw.location.sntc_id;
              Lwt_io.printl sntc_sep
              >> pp_wrap ~print_location ~show_token tw
            end else
              pp_wrap ~print_location ~show_token tw
        ) token_wraps
    
  end

  (*Std. version of this module - should be defined from CLI-args
    > specifically the equal function should be partially applied
      from args
  *)
  module Eq_TokenWrap :
    (EQ_TOKENWRAP with type t = T.token_wrap
                   and type score = T.score) =
  struct
    type t = TokenWrap.t
    type score = float

    let equal_loose t t' =
      let token = TokenWrap.token in
      Cmp_token.TokenCmpLoose.equal
        (*goto> these args should be controlled from CLI*)
        ~verbose:false
        ~settings:Cmp_token.std_cmp_settings
        (token t) (token t')

    let compare_score = Float.compare
    let show_score = Float.to_string 

  end

  
  module Ins = struct 

    let stats_v1 ~db ~anal_id ~tokens ~tid1 ~tid2 = 
      Sqex.execute db
        [%sqlc "INSERT OR REPLACE INTO 
              stats(anal_id, doc1_id, doc2_id, timestamp, 
                    nmatch_tokens, match_tokens_json) 
              VALUES(%d, %d, %d, %d, %d, %s)" ]
        anal_id 
        tid1 tid2 
        (Int.of_float (Unix.time())) 
        (List.length tokens)
        (Token.to_json_anon tokens)

  end

  module Sel = struct 

    let select_all_sections db = 
      Sqex.select db
        [%sqlc "select @d{fk_documentID}, @s{group_concat(content,\". \")}
                from titles
                where anonymous = 0
                group by fk_documentID" ]
      >>= Lwt_list.map_s
        (fun (id,text) -> Lwt.return {id; text})

    (*goto design a test for extracting correctly*)
    (*goto: extract page-id's*)
    let select_section ?(section_id=19) db = 
      Sqex.select db 
        [%sqlc "SELECT @d{fk_documentID},@s{content} FROM titles 
             WHERE name IN
               (
                 SELECT identifier FROM temp_titles 
                 WHERE ID = %d
               )
             ORDER BY pagenumber DESC" ] 
        section_id
      >>= Lwt_list.map_s
        (fun (id,text) -> Lwt.return {id; text})

    module IM = 
      Map.Make (struct
        type t = text_id
        let compare = compare
      end) 

    let concat_texts_pr_doc l =
      List.fold_right 
        (fun {id; text} acc ->
           IM.add id 
             (try (IM.find id acc) ^ "\n\n\n" ^ text
              with Not_found -> text
             ) acc
        ) l IM.empty
      |> IM.enum
      |> Enum.map (fun (id, text) -> {id; text})
      |> List.of_enum

    (*goto: we need to save location id's for pages of sections*)
    let section_concat ?(section_id=19) db = 
      select_section ~section_id db
      >|= concat_texts_pr_doc (*concatting sections split over several pages*)

    let print_section_ids ss = 
      Lwt_io.printf "Selecting sections: [" >>
      Lwt_list.iter_s (fun id -> Lwt_io.printf " %d;" id) ss >>
      Lwt_io.printl "]" 


    (*goto makes more sense to limit the textual length of cat sect's than limit count*)
    let texts ?(limit_pr_sect=None) ~sections db = 
      match sections with 
      | `All -> 
        Lwt_io.printl "Selecting all sections.." >>
        select_all_sections db
      | `List section_ids ->
        print_section_ids section_ids >>
        Lwt_list.map_s (fun id -> 
            section_concat ~section_id:id db
          ) section_ids
        >|= List.flatten
        (*goto: we need to save location id's for sections in doc *)
        >|= concat_texts_pr_doc (*concatting sections*)

  end

  module Init = struct 

    let stats db = 
      Sqex.execute db
        [%sqlinit "CREATE TABLE IF NOT EXISTS stats (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 anal_id INTEGER NOT NULL,
                 doc1_id INTEGER NOT NULL,
                 doc2_id INTEGER NOT NULL,
                 timestamp INTEGER NOT NULL,
                 nmatch_tokens INTEGER NOT NULL,
                 match_tokens_json TEXT NOT NULL
            );" ]

    let protostats db = 
      Sqex.execute db
        [%sqlinit "CREATE TABLE IF NOT EXISTS protostats (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 anal_id INTEGER,
                 content TEXT,
                 timestamp INTEGER
            );" ]

  end

end

module PompV2 = struct 

  module T = struct

    type 'a filter = [ `All | `List of 'a list ]
    
    type filters = {
      datasets : int filter (*id*);
      docs : int filter (*id*);
      sects : int filter (*id*);
    }

    type text_id = int

    type text_part = {
      part_id : int;
      sect_id : int;
      part : string;
    }
    
    type text_content = text_part list (*rem use assoc funcs.*)

    (*goto save dataset id too? 
      (text_id maps in db to dataset.. - just for printing)
      > else we just lookup in db with some CB / other *)
    type text_entry = {
      text_id : text_id;
      parts_ids : int list;
      sects_ids : int list;
      text : text_content;
    }

    type token_location_scope = {
      part_id : int;
      sect_id : int;
      text_id : int;
    }

    type token_location = {
      token_id : int;
      sntc_id : int;
      part_id : int;
      sect_id : int;
      text_id : int;
    } [@@deriving ord]
    
    type token_wrap = {
      token : Token.t;
      location : token_location;
    }

    type score = float

  end
  open T

  
  module TextEntry :
    (TEXTENTRY
     with type id = T.text_id
      and type t = T.text_entry) =
  struct

    type id = T.text_id
    type content = T.text_content
    type t = T.text_entry
    
    let id ({text_id}:text_entry) = text_id
    let text {text} = text

    let show_id = Int.to_string
    let show_text content =
      content
      |> List.map (fun {part} -> part)
      |> String.concat " "
    
  end

  
  module TokenWrap :
    (TOKENWRAP with type t = T.token_wrap
                and type text_entry = T.text_entry
                and type location = T.token_location) =
  struct
    type t = token_wrap
    type location = token_location
    type location_info = token_location_scope

    let token {token} = token
    let location {location} = location 
    let wraps_of_sntcs
        ({part_id; sect_id; text_id}:token_location_scope)
        sentences =
      let sentence_to_wraps sntc_id =
        List.mapi (fun token_id token ->
            { token;
              location =
                { token_id; sntc_id; part_id; sect_id; text_id }}
          )
      in
      sentences      
      |> List.mapi sentence_to_wraps 
      |> List.flatten
    
    type text_entry = T.text_entry

    (*goto test*)
    let text_to_wraps {text_id; text} =
      let part_to_wraps {part_id; sect_id; part} = 
        Tokenizer.of_string part
        |> wraps_of_sntcs {part_id; sect_id; text_id}
      in
      text
      |> List.map part_to_wraps 
      |> List.flatten
           
    let pp_wrap
        ({token;
          location={token_id; sntc_id; part_id; sect_id; text_id}})
        ~print_location ~show_token
      =
      match print_location with
      | false -> Lwt_io.printf "%s " (show_token token)
      | true -> begin
          Lwt_io.printf
            " { token = \"%s\"; token_id = %d; sentence_id = %d; \
                part_id = %d; sect_id = %d; text_id = %d }\n"
            (show_token token)
            token_id
            sntc_id
            part_id
            sect_id
            text_id
        end

    let pp_wraps ?(print_location=false) ~show_token ~sntc_sep token_wraps =
      let last_sntc = ref None in
      Lwt_list.iter_s (fun tw ->
          match !last_sntc with
          | None -> pp_wrap ~print_location ~show_token tw
          | Some last_sntc_id ->
            if tw.location.sntc_id <> last_sntc_id then begin
              last_sntc := Some tw.location.sntc_id;
              Lwt_io.printl sntc_sep
              >> pp_wrap ~print_location ~show_token tw
            end else
              pp_wrap ~print_location ~show_token tw
        ) token_wraps    
    
  end

  (*Std. version of this module - should be defined from CLI-args
    > specifically the equal function should be partially applied
      from args
  *)
  module Eq_TokenWrap :
    (EQ_TOKENWRAP with type t = T.token_wrap
                   and type score = T.score) =
  struct
    type t = TokenWrap.t
    type score = T.score

    let equal_loose t t' =
      let token = TokenWrap.token in
      Cmp_token.TokenCmpLoose.equal
        (*goto> these args should be controlled from CLI*)
        ~verbose:false
        ~settings:Cmp_token.std_cmp_settings
        (token t) (token t')

    let compare_score = Float.compare
    let show_score = Float.to_string 

  end

  module Sel = struct 

    (*We call this either once or several times, depending on filters*)
    let sections ~datasets ~docs ~sects db =
(*
      let s = function
        | Some i -> "\"Some "^Int.to_string i^"\""
        | None -> "None"
      in
      Lwt_io.printf ">> selecting : dataset %s, doc %s, sect %s >>\n"
        (s datasets) (s docs) (s sects)
      >>
*)
      Sqex.select db 
        [%sqlc
          "select @d{structurizer_documents_id},
                  @d{structurizer_titles_id},
                  @d{template_titles_id},
                  @s{st.content}
           from structurizer_titles as st
           join structurizer_documents as sd
             on st.fk_structurizer_documents = sd.structurizer_documents_id
           join structurizer_settings as ss
             on sd.fk_structurizer_settings = structurizer_settings_id
           join dataset_files as df
             on df.dataset_files_id = ss.fk_dataset_files
           join dataset as ds
             on ds.dataset_id = df.fk_dataset
           join template_titles as tt
             on st.name = tt.identifier
           where (%d? IS NULL OR ds.dataset_id = %d?)
             and (%d? IS NULL OR sd.structurizer_documents_id = %d?)
             and (%d? IS NULL OR tt.template_titles_id = %d?)
             and anonymous = 0 
           order by st.structurizer_titles_id"
        ]
        datasets datasets
        docs docs
        sects sects
    (*< needed this null-check with sqlexpr as we cannot construct 
        strings with this lib at runtime*)
    (*
      >>= fun list -> 
      Lwt_list.iter_s (fun (_,pid,_,s) -> Lwt_io.printf "part_id:%d, content: %s\n" pid s) list >>
      Lwt.return list
    *)

    module TMap = 
      Map.Make (struct
        type t = T.text_id
        let compare = Int.compare
      end)

    let text_entry_of_section
        (doc_id, tit_id, templ_tit_id, content)
        acc_texts
      = 
      let part =
        { part_id = tit_id
        ; sect_id = templ_tit_id
        ; part = content } in
      TMap.modify_opt doc_id
        (function
          | Some t -> 
            Some { t with
                   parts_ids = tit_id::t.parts_ids
                 ; sects_ids = templ_tit_id::t.sects_ids
                 ; text = part::t.text }
          | None ->
            Some { text_id = doc_id
                 ; parts_ids = [tit_id]
                 ; sects_ids = [templ_tit_id]
                 ; text = [part] }            
        ) acc_texts
    

    let text_entries_of_sections sections =
      List.fold_right text_entry_of_section
        sections TMap.empty
      |> TMap.values
      |> List.of_enum
      |> List.map (fun t ->
          { t with
            parts_ids = List.sort_uniq Int.compare t.parts_ids
          ; sects_ids = List.sort_uniq Int.compare t.sects_ids }
        )
    
    (*with semantics where None = All, see 'sections'*)
    let apply_filter filter f =
      match filter with 
      | `All -> f None 
      | `List l ->
        Lwt_list.map_s (f%Option.some) l >|= List.flatten


    let texts ~filters db = 
      apply_filter filters.datasets @@ fun datasets ->
      apply_filter filters.docs @@ fun docs ->
      apply_filter filters.sects @@ fun sects ->
      sections ~datasets ~docs ~sects db
      >|= text_entries_of_sections

  end

  (*>goto implement new db structure procs*)
  module Init = struct 

  end

  module Ins = struct 

    open Lwt 

    (*goto test with current db*)
    let clear_aux db = function
      | `TokenWraps ->
        Sqex.execute db [%sql "DELETE FROM analytics_tokens" ] >>
        Sqex.execute db [%sql "DELETE FROM analytics_sentences" ]
      | `TxtMatches ->
        Sqex.execute db [%sql "DELETE FROM analytics_matches_tokens" ] >>
        Sqex.execute db [%sql "DELETE FROM analytics_matches_docs" ]

    let clear db = Lwt_list.iter_p @@ clear_aux db

    module SMap = 
      Map.Make (struct
        type t = int (*part_id*) * int (*sntc_id*)
        let compare = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare
      end)

    module TWMap =
      Map.Make (struct
        type t = T.token_location
        let compare = T.compare_token_location
      end)

    (*goto test for stack overflow fold left / on reversed list
    *)
    (*goto test more*)
    let tokenwraps ~analysis_id ~db tokenwraps =
      Lwt_list.fold_left_s
        (fun (sntc_db_ids, tw_db_ids) tw ->
           let sntc_loc = (tw.location.part_id, tw.location.sntc_id) in
           begin
             match SMap.find sntc_loc sntc_db_ids with
             | sntc_db_id -> Lwt.return (sntc_db_ids, sntc_db_id) 
             | exception Not_found ->
               (*>goto add db column for sntc pos in title*)
               Sqex.insert db
                 [%sqlc
                   "insert into 
                      analytics_sentences(fk_analytics, fk_structurizer_titles)
                      values(%d, %d)" ]
                 analysis_id tw.location.part_id
               >|= fun sntc_db_id ->
               (SMap.add sntc_loc sntc_db_id sntc_db_ids,
                sntc_db_id)
           end
           >>= fun (sntc_db_ids, sntc_db_id) -> 
           Sqex.insert db
             [%sqlc
               "insert into 
                  analytics_tokens(fk_analytics_sentences, type, value)
                  values(%L, %s, %s)" ]
             sntc_db_id
             (Token.type_string tw.token)
             (Token.to_string tw.token)
           >|= fun tw_id ->
           let tw_db_ids = TWMap.add tw.location tw_id tw_db_ids in
           (sntc_db_ids, tw_db_ids)
        ) (SMap.empty, TWMap.empty) tokenwraps 
      >|= fun (_, tw_db_ids) -> tw_db_ids

    (*goto test*)
    let txtmatches ~db ~tokenwrap_ids ~analysis_id txtmatches = 
        Lwt_list.iter_s (fun ((tx1id, tx2id, score), tokenmatches) ->
          Sqex.insert db
            [%sqlc
               "insert into 
                  analytics_matches_docs(fk_doc1, fk_doc2, score)
                  values(%d, %d, %f)"]
            (*analysis_id*)
            tx1id tx2id score
          >>= fun txtm_db_id ->
          Lwt_list.iter_s (fun (tw, tw', tw_score) -> 
              Sqex.execute db
                [%sqlc
                  "insert into
                     analytics_matches_tokens(
                       fk_analytics_matches_docs, 
                       fk_token_1, fk_token_2, score
                     )
                     values(%L, %L, %L, %f)"]
                txtm_db_id
                (TWMap.find tw.location tokenwrap_ids)
                (TWMap.find tw'.location tokenwrap_ids)
                tw_score
            ) tokenmatches
        ) txtmatches 
    
  end


  
end



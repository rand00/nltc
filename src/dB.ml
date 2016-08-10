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
end

module type TOKENWRAP = (*functor (Eq:EQ) ->*)
sig
  type t
  type location
  type location_info
  
  val token : t -> Token.t
  val location : t -> location
  val wraps_of_sntcs : location_info -> Token.t list list -> t list

  type text_entry
  val text_to_wraps : text_entry -> t list
  (*<gomaybe: later: can be parametrized with tokenizer if we construct 
    diff. versions*)
end


(*goto : implement with a callback interface for analysis as well?*)
(** Local db for importing text through cmdline interface*)
module Local = struct

  module T = struct 

    type text_id = string
    type text_content = string
    type text_entry = {
      filename : text_id; 
      text : text_content;
    }
    type token_wrap = Token.t
    type score = float

  end
  (* include T *)
  open T

  module TextEntry :
    (TEXTENTRY
     with type id := T.text_id
      and type t := T.text_entry
      and type content := T.text_content) =
  struct

    type id = T.text_id
    type t = T.text_entry
    type content = T.text_content
    
    let id {filename} = filename
    let text {text} = text
    
  end

  module TokenWrap_Naked :
    (TOKENWRAP
     with type t = T.token_wrap
      and type text_entry = T.text_entry) =
  struct

    type t = T.token_wrap
    type location = unit
    type location_info = unit

    let token t = t
    let location t = ()
    let wraps_of_sntcs () = List.flatten 
    
    type text_entry = T.text_entry
    let text_to_wraps {text} = 
      Tokenizer.of_string text |> List.flatten

  end

  (*Std. version of this module - should instead be defined from CLI-args*)
  module Eq_TokenWrap :
    (EQ_TOKENWRAP with type t = T.token_wrap
                   and type score = T.score) =
  struct

    type t = T.token_wrap
    type score = T.score

    let equal_loose t t' =
      let token = TokenWrap_Naked.token in
      Cmp_token.TokenCmpLoose.equal
        (*goto> these args should be controlled from CLI*)
        ~verbose:false
        ~settings:Cmp_token.std_cmp_settings
        (token t) (token t')

  end
  
  let init_tables db =
    Sqex.execute db
      sqlinit"CREATE TABLE IF NOT EXISTS documents (
                 id INTEGER PRIMARY KEY,
                 text TEXT,
                 filename TEXT UNIQUE
            );"

  let del_docs db = 
    Sqex.execute db sql"DELETE FROM documents"

  module Ins = struct 

    (** Overwrites text that comes from files of the same name *)
    let replace_filetext =
      sqlc"INSERT OR REPLACE INTO documents(filename, text) VALUES(%s, %s)"

  end

  module Sel = struct 

    let texts db = 
      Sqex.select db sqlc"SELECT @s{filename},@s{text} FROM documents"
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
    type token_wrap = Token.t
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
  (* include T *)

  module TextEntry :
    (TEXTENTRY
     with type id = T.text_id
      and type t = T.text_entry) =
  struct

    type id = T.text_id
    type content = T.text_content
    type t = T.text_entry
    
    let id {id} = id
    let text {text} = text
    
  end

  module TokenWrap_Naked :
    (TOKENWRAP with type t = T.token_wrap) =
  struct
    type t = T.token_wrap
    type location = unit
    type location_info = unit

    let token t = t
    let location t = ()
    let wraps_of_sntcs () = List.flatten 

    type text_entry = T.text_entry
    let text_to_wraps {text} =
      Tokenizer.of_string text |> List.flatten

  end

  (*Std. version of this module - should be defined from CLI-args
    > specifically the equal function should be partially applied
      from args
  *)
  module Eq_TokenWrap :
    (EQ_TOKENWRAP with type t := TokenWrap_Naked.t
                   and type score := T.score) =
  struct
    type t = TokenWrap_Naked.t
    type score = float

    let equal_loose t t' =
      let token = TokenWrap_Naked.token in
      Cmp_token.TokenCmpLoose.equal
        (*goto> these args should be controlled from CLI*)
        ~verbose:false
        ~settings:Cmp_token.std_cmp_settings
        (token t) (token t')
  end

  
  module Ins = struct 

    let stats_v1 ~db ~anal_id ~tokens ~tid1 ~tid2 = 
      Sqex.execute db
        sqlc"INSERT OR REPLACE INTO 
              stats(anal_id, doc1_id, doc2_id, timestamp, 
                    nmatch_tokens, match_tokens_json) 
              VALUES(%d, %d, %d, %d, %d, %s)"
        anal_id 
        tid1 tid2 
        (Int.of_float (Unix.time())) 
        (List.length tokens)
        (Token.to_json_anon tokens)

  end

  module Sel = struct 

    let select_all_sections db = 
      Sqex.select db
        sqlc"select @d{fk_documentID}, @s{group_concat(content,\". \")}
             from titles
             where anonymous = 0
             group by fk_documentID"
      >>= Lwt_list.map_s
        (fun (id,text) -> Lwt.return {id; text})

    (*goto design a test for extracting correctly*)
    (*goto: extract page-id's*)
    let select_section ?(section_id=19) db = 
      Sqex.select db 
        sqlc"SELECT @d{fk_documentID},@s{content} FROM titles 
             WHERE name IN
               (
                 SELECT identifier FROM temp_titles 
                 WHERE ID = %d
               )
             ORDER BY pagenumber DESC" 
        section_id
      >>= Lwt_list.map_s
        (fun (id,text) -> Lwt.return {id; text})

    module IM = 
      Map.Make (struct
        type t = text_id
        let compare = compare
      end) 
    (*      : Map.S with type key = text_id *)

    (*goto remove?*)
    (*open Text_entry *)

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
    let sections ?(limit_pr_sect=None) ~sections db = 
      match sections with 
      | `All -> 
        Lwt_io.printl "Selecting all sections.." >>
        select_all_sections db
      | `Sections section_ids ->
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
        sqlinit"CREATE TABLE IF NOT EXISTS stats (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 anal_id INTEGER NOT NULL,
                 doc1_id INTEGER NOT NULL,
                 doc2_id INTEGER NOT NULL,
                 timestamp INTEGER NOT NULL,
                 nmatch_tokens INTEGER NOT NULL,
                 match_tokens_json TEXT NOT NULL
            );"

    let protostats db = 
      Sqex.execute db
        sqlinit"CREATE TABLE IF NOT EXISTS protostats (
                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                 anal_id INTEGER,
                 content TEXT,
                 timestamp INTEGER
            );"

  end

end

module PompV2 = struct 

  module T = struct

    type filter = [ `All | `List of int list ]
    
    type filters = {
      docs : filter;
      sects : filter;
    }

    type text_id = int

    type text_part = {
      part_id : int;
      sect_id : int;
      part : string;
    }
    
    type text_content = text_part list (*rem use assoc funcs.*)
    
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
    }
    
    type token_wrap = {
      token : Token.t;
      location : token_location;
    }

    type score = float

  end
  open T
  (* include T *)

  
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

  end

  
  module TokenWrap :
    (TOKENWRAP with type t = T.token_wrap) =
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

  end

  (*Std. version of this module - should be defined from CLI-args
    > specifically the equal function should be partially applied
      from args
  *)
  module Eq_TokenWrap :
    (EQ_TOKENWRAP with type t := TokenWrap.t
                   and type score := T.score) =
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
  end


  module Ins = struct 

  end

  module Sel = struct 

    (*goo*)
    (*goto implement this, making use of V1 sections concatting etc
      . read db structure with sqlite manager
      . make selector
      . put v1-combinators in different scope (taking selector-func as arg)?
      . compose v1-combinators here
      . test with Cli_pomp_run clijob *)
    let sections ~sections db = assert false

  end

  module Init = struct 

  end
  (*goto implement new db structure procs*)


end



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
open Token

(**Helper functions*)
let regexp = Pcre.pmatch 

(**Word-char types for parsing*)

type pararef = { 
  parasym : string;
  paranum : string
}

type wrd_pos = 
  | Wrd_fin (*there a no more char's in current word (and no new word to start)*)
  | Wrd_new of string (*current word (if exists) should get saved, and new word startet with new char*)
  | Wrd_pre_new of string * string (*when having looked forwards > found new word*)
  | Wrd_pre_last_new of string * string (*when having looked forwards + new sentence*)
  | Wrd_last_new of string 
  | Wrd_unfin of string (*current word gets appended a new char*)
  | Wrd_para of pararef (*when having found a paragraph reference, which are parsed differently*)
  | Wrd_last (*last word in sentence*)

(**Types of tokens that get collected in sentence-lists*)
type token = Token.token

(** -------------------------------parser begin---------------------------------------*)

(** recursive descent parser, from char(singleton-string) Stream*)
let rec tokenizer ~ch_acc ~wrd_acc ~snt_acc = parser
    | [< wrd_pos = char_parser; 

         _expr_ = (match wrd_pos with 

	 | Wrd_fin -> (match ch_acc with

	   | "" -> tokenizer ~ch_acc:"" ~wrd_acc ~snt_acc
	   | _  -> tokenizer ~ch_acc:"" ~wrd_acc:((Word ch_acc)::wrd_acc) ~snt_acc)

         | Wrd_new ch -> tokenizer ~ch_acc:ch 
           ~wrd_acc:(match ch_acc with 
           | "" -> wrd_acc
           | _  -> (Word ch_acc)::wrd_acc)
           ~snt_acc
           
         | Wrd_pre_new (ch_pre, ch_new) -> 
           let acc_pre = ch_acc ^ ch_pre in
           (match acc_pre with 
           | "" -> tokenizer ~ch_acc:ch_new ~wrd_acc ~snt_acc
           | _  -> tokenizer ~ch_acc:ch_new ~wrd_acc:((Word acc_pre)::wrd_acc) ~snt_acc)

         | Wrd_pre_last_new (ch_pre, ch_new) ->
           let acc_pre = ch_acc ^ ch_pre in
           (match acc_pre with 
           | "" -> (tokenizer ~ch_acc:ch_new ~wrd_acc:[] 
                      ~snt_acc:(match wrd_acc with 
                      | [] -> snt_acc
                      | _  -> wrd_acc::snt_acc))
           | _  -> (tokenizer ~ch_acc:ch_new ~wrd_acc:[] 
                      ~snt_acc:(((Word acc_pre)::wrd_acc)::snt_acc)))

         | Wrd_last_new ch_new -> 
           (tokenizer ~ch_acc:"" ~wrd_acc:[]
              ~snt_acc:(match ch_acc with
              | "" -> 
                (match wrd_acc with
                | [] -> snt_acc
                | _  -> wrd_acc::snt_acc)
              | _ -> ((Word ch_acc)::wrd_acc)::snt_acc))

         | Wrd_unfin ch -> tokenizer ~ch_acc:(ch_acc^ch) ~wrd_acc ~snt_acc
           
         | Wrd_para para -> 
           (tokenizer ~ch_acc:"" 
              ~wrd_acc:(match ch_acc with
              | "" -> (Pararef (para.parasym,para.paranum)) :: wrd_acc 
              | _  -> (Pararef (para.parasym,para.paranum)) :: (Word ch_acc) :: wrd_acc)
              ~snt_acc)

	 | Wrd_last -> (match ch_acc with 
	   | "" -> (tokenizer ~ch_acc:"" ~wrd_acc:[] 
                      ~snt_acc:(match wrd_acc with
		      | [] -> snt_acc
		      | _  -> wrd_acc::snt_acc))
	   | _  -> (tokenizer ~ch_acc:"" ~wrd_acc:[] 
                      ~snt_acc:(((Word ch_acc)::wrd_acc)::snt_acc))))

      >] -> _expr_

      | [< >] -> (match ch_acc with 
        | ""  -> List.(rev (map rev (wrd_acc::snt_acc)))
        | _   -> List.(rev (map rev (((Word ch_acc)::wrd_acc)::snt_acc))))

(** ---------------------------parsing of all chars----------------------------------------*)

(*goto > continiously test all the code starting from [char_parser]*)
and char_parser = parser
    | [< e = visual_parser >] -> e
    | [< e = whitespace_only_parser >] -> e

(** -------------------------------all visual chars---------------------------------------*)

(*goto > add case for symbols! (last case)  < checks for sentence end (dots, exclamation, question...) *)

and visual_parser = parser
    | [< e = word_parser >] -> e
    | [< e = pararef_parser >] -> e
    | [< e = symbol_parser >] -> e

(** -------------------------------word parser--------------------------------------------*)      
(*goto > when implementing call for visual parser - refactor these functions?*)

and word_parser = parser
    | [< 'c when c |> Otext.is_alpha; e = dot_or_dash_parser c >] -> e

(*todo > maybe separate these two cases out into to functions - where the dash
         part then can be used in the symbol-parser; but maybe not; the dash 
         could mean a 'thoughstroke' (danishification) if there is a space between
         word and the dash
*)
and dot_or_dash_parser ch_acc = parser
    (*keeps dot's in words*)
    | [< 'c when c = "."; e2 = whitespace_parser ""; e3 = dot_continue_parser e2 >] -> 
      ( match e3 with 
      | Wrd_new c'      -> Wrd_pre_new ((ch_acc ^ c), c')
      | Wrd_unfin c'    -> Wrd_unfin (ch_acc ^ c ^ c')
      | Wrd_last_new c' -> Wrd_pre_last_new (ch_acc, c')
      | _               -> Wrd_unfin ch_acc )
    (*deletes dash + whitespace from words*)
    | [< 'c when c = "-"; e2 = whitespace_parser ""; e3 = dash_continue_parser e2 >] -> 
      ( match e3 with 
      | Wrd_unfin c'    -> Wrd_unfin (ch_acc ^ c')
      | Wrd_last_new c' -> Wrd_pre_last_new (ch_acc, c')
      | _               -> Wrd_pre_last_new (ch_acc, "") ) 
    (*goto > 
      . test what happens here with symbols (after the dot)!
        > if symbol gets swallowed, then call char_parser here (or visual - as whitespace has been checked)
          > maybe handle this char with char_parser..?
    *)

    (*case: there is no dot/dash in connection with :alpha:-char*)
    | [< >] -> Wrd_unfin ch_acc 

and dot_continue_parser whitespace_return = parser
    (*if next word is lower-case, then check for size of whitespace, and decide sentence boundary*)
    | [< 'c when c |> Otext.is_lower; e = dotwrd_dot_parser c >] -> 
      ( match whitespace_return with 
      | Wrd_unfin _ -> Wrd_unfin e 
      | Wrd_fin     -> Wrd_new e
      | _           -> Wrd_last_new e ) 
    (*if next word is upper-case, then check for size of whitespace, and decide sentence boundary*)
    | [< 'c when c |> Otext.is_upper; e = dotwrd_dot_parser c >] -> 
      ( match whitespace_return with 
      | Wrd_unfin _ -> Wrd_unfin e
      | _           -> Wrd_last_new e )
    (*goto > call visual- / char-parser AND handle return-val of that?*)      

    (*case where stream is empty*)
    | [< >] -> Wrd_last

(** These two following functions recursively itererates through any dot-wrd until 
    some char that isn't dot/alpha is found.
*)
and dotwrd_dot_parser acc = parser 
    | [< 'c when c = "."; e = dotwrd_alph_parser (acc ^ c) >] -> e
    | [< >] -> acc

and dotwrd_alph_parser acc = parser
    | [< 'c when c |> Otext.is_alpha; e = dotwrd_dot_parser (acc ^ c) >] -> e
    | [< 'c when c |> Otext.is_alpha; e = dotwrd_alph_parser (acc ^ c) >] -> e
    | [< >] -> acc

and dash_continue_parser whitespace_returnval = parser
    | [< 'c when c |> Otext.is_alpha >] -> ( match whitespace_returnval with 
      | Wrd_fin  | Wrd_unfin _ -> Wrd_unfin c
      | _ -> Wrd_last_new c ) 
    (*goto > call visual- / char-parser AND handle return-val of that?*)

    (*case where stream is empty*)
    | [< >] -> Wrd_last


(** -------------------------------paragraf references------------------------------------*)      
(*goto [tokenizer] <- add check for "sektion, paragraf etc.." ved hver word finished*)

(** Parsing of references to specific paragraphs *)
and pararef_parser = parser 
    | [< p_b = pararef_begin; w_s = whitespace_parser ""; p_n = pararef_num "" >] -> 
      (match w_s with 
      | Wrd_fin ->
        (match p_n with
        | "" -> Wrd_pre_new (p_b, "")
        | _  -> Wrd_para {parasym = p_b; paranum = p_n}) 
      | Wrd_unfin _ -> 
        (match p_n with
        | "" -> Wrd_unfin p_b
        | _  -> Wrd_para {parasym = p_b; paranum = p_n})
      | _ -> Wrd_pre_last_new (p_b, p_n))

(**parsing (detection) of the paragraph reference symbols*)

and is_parasym0 = regexp ~rex:<:re< "§" >>
and is_parasym1 = regexp ~rex:<:re< "¶" >>

and pararef_begin = parser
    | [< 'c when c |> is_parasym0; e = pararef_sym0more c >] -> e
    | [< 'c when c |> is_parasym1; e = pararef_sym1more c >] -> e

and pararef_sym0more acc = parser
    | [< 'c when c |> is_parasym0; e = pararef_sym0more (acc^c) >] -> e 
    | [< >] -> acc 

and pararef_sym1more acc = parser
    | [< 'c when c |> is_parasym1; e = pararef_sym1more (acc^c) >] -> e
    | [< >] -> acc 

(**parsing of the paragraph-number referenced*)
and is_num = regexp ~rex:<:re< [ "0" - "9" ] >>
and is_alnum = Otext.is_alnum

and pararef_num acc = parser
    | [< 'c when c |> is_num; e = pararef_num_more c >] -> e
    | [< >] -> acc 

and pararef_num_more acc = parser
    | [< 'c when c |> is_alnum; e = pararef_num_more (acc^c) >] -> e
    | [< >] -> acc 

(** ---------------------------------symbols----------------------------------------*)

(*goto > think of all the cases for symbols where you would wan't to save them
  > either as part of word (@ in a mail-adr) or as new seperate word..
*)

and is_symb c = if c |> Otext.is_graph then not (c |> Otext.is_alpha) else false
and is_sntc_end = regexp ~rex:<:re< [".!?"] >>

and symbol_parser = parser 
    | [< 'c when c |> is_sntc_end >] -> Wrd_last
    | [< 'c when c |> is_symb >] -> Wrd_fin (*goto > make new wrd_pos type for symbols?*)

(** -------------------------------whitespace---------------------------------------*)

and is_whitespace = regexp ~rex:(<:re< [" " "\n" "\r" "\t"] >>)

and whitespace_only_parser = parser 
    | [< 'c when c |> is_whitespace ;e = whitespace_parser c >] -> e

and whitespace_parser acc = parser 
    | [< 'c when c |> is_whitespace ;e = whitespace_parser (acc^c) >] -> e
    | [< >] -> match acc with
      | <:re< [ " \r\n\t" ]{3+} >> -> Wrd_last
      | <:re< "  " | "\r\n" >>     -> Wrd_fin
      | <:re< [ " \r\n\t" ]{2} >>  -> Wrd_last
      | <:re< [ " \r\n\t" ]{1} >>  -> Wrd_fin
      | "" | _ -> Wrd_unfin acc 

(** ===============================parser end=======================================*)


(** call one of these functions for parsing*)
let of_stream = tokenizer ~ch_acc:"" ~wrd_acc:[] ~snt_acc:[] 

let of_string s = s |> Otext.to_stream |> of_stream

(**tokenizer output handlers*)
let reprint snt_sep format = List.(
  iter (fun sntc ->
    print_string snt_sep;
    iter (fun t -> Printf.printf "%s " (format t)) sntc
  ))

let reprint_lwt snt_sep format = Lwt_list.(
  iter_s (fun sntc ->
    Lwt_io.print snt_sep
    >> iter_s (Lwt_io.printf "%s " % format) sntc
  ))



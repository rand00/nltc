
(** Type of tokens that get's added to the document comparator-sets *)

(*
type token_type = 
  | Word
  | Word_para
  | Num
  | Alnumdot
  | Pararef
  | Email
  [@@deriving ord]

type 'a token =

(*< is this practical at all, as we can't save diff types inside..*)
*)  

type token = 
  | Word of string
  | Word_para of string
  | Num of string
  | Alnumdot of string
  | Pararef of string * string
  | Email of string

(*<gomaybe change this type -> see example:

  type a = A | B
  type 'a foo = a * 'a

  let a : string foo = A, "koq" 
  and b : int foo = B, 2 ;;
  val a : bytes foo = (A, "koq")
  val b : int foo = (B, 2)
*)
(*<goto extend with ppx [ord; show; eq?; yojson?; ]
*)


type token_wrap = {
  token : token;   (*content of token*)
  id_wrd  : int;      (*pr. sentence word id*)
  id_sntc : int;     (*pr. 'parsed stream' sentence id*)
  id_db_part : int option;   (*what is called .. 'titles' in db *)
  id_doc : [ `Id of int | `Name of string ] option;
} [@@deriving make]

let example_token_wrap () =
  let t = { token = Word "kd"; id_wrd = 4; id_sntc = 44; id_db_part = None; id_doc = None}
  and t' = make_token_wrap ~token:(Word "kd") ~id_wrd:4 ~id_sntc:44 ()
  in ()


type t = token

let to_tstring = function 
  | Word s       -> "Word(" ^ s ^ ")"
  | Word_para s  -> "Word_para(" ^ s ^ ")"
  | Num s        -> "Num(" ^ s ^ ")"
  | Alnumdot s   -> "Alnumdot(" ^ s ^ ")"
  | Email s      -> "Email(" ^ s ^ ")"
  | Pararef (s,s') -> "Pararef(" ^s^ " " ^s'^ ")"

let to_tstring_anon = function 
  | Word s
  | Word_para s
  | Num s      
  | Alnumdot s 
  | Email s      -> s
  | Pararef (s,s') -> s^" "^s'

let to_string = to_tstring_anon

let to_json_aux to_str_fun ts = 
  let module Y = Yojson.Basic in
  (`Assoc [ 
    ("tokens",
     (`List (List.map (fun t -> 
       `String (to_str_fun t)
      ) ts ))) ])
  |> Y.pretty_to_string

let to_json = 
  to_json_aux to_tstring

let to_json_anon = 
  to_json_aux to_tstring_anon



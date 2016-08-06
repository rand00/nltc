(*ID's of stuff to be compared*)
(*goto this now needs to save the exact info about which 
  . sections that were included
    . titles that where included
  . and id of what where compared - e.g. docID/name
*)

(** For specification and results of comparisons : ID only types*)

type sections = [
  | `All
  | `Sections of int list
]

(*goto see experiment location_type_extensible *)




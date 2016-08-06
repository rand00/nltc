

type analysis_settings = {
  mode : [`Strict | `Loose]; 
  exclude_strings : string list;
  (*> only possible with cmp_with_lists (n^2). Int means how many words
    max can be between matches*)
  (*> goto is this going to be used?*)
  words_in_seq_maxdist : int option; 
}

(**Std. analysis settings*)
(*The analysis is at the curren point in time not making full usage of
  these settings - these are implemented to be able to easily extend
  the module with the wished-for functionality*)

let std_analysis = {
  mode = `Loose;
  (*> goto implement and use*)
  exclude_strings = [];
  words_in_seq_maxdist = None;
} 

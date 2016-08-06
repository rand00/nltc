

open Cmp_token

let equal = WordCmpLoose.equal_score


let _ = 
  let settings = ref WordCmpLoose.wset in
  let program_name = 
    let n = Filename.basename Sys.argv.(0) in
    let n = 
      try Filename.chop_extension n 
      with Invalid_argument _ -> n in
    n 
  in
  let anon_pos = ref 0 in
  let w0 = ref ""
  and w1 = ref ""
  and verbose = ref false
  in 
  Arg.parse 
    (Arg.align ~limit:2
       [ 
         begin "--print-settings", 
               Arg.Unit (fun () -> print_endline @@ str_of_wset !settings; exit 0),
               ": Prints the standard settings of ["^
               program_name^"]"
         end;
         begin "-v", 
               Arg.Unit (fun () -> verbose:=true),
               ": Sets verbose mode; outputs more info about the word match."
         end;
         begin "--set-goal", 
               Arg.Float (fun goal -> 
                   settings := { !settings with goal }),
               ": Settings modification - sets the goal."
         end;
         begin "--set-weight_eq", 
               Arg.Float (fun weight_eq -> 
                   settings := { !settings with weight_eq }),
               ": Settings modification - sets the weight of equal \
                  chars in middle of word."
         end;
         begin "--set-weight_endeq", 
               Arg.Float (fun weight_endeq -> 
                   settings := { !settings with weight_endeq }),
               ": Settings modification - sets the weight of equal \
                  chars in end of word."
         end;
         begin "--set-weight_wordlen", 
               Arg.Float (fun weight_wordlen -> 
                   settings := { !settings with weight_wordlen }),
               ": Settings modification - sets the weight of equal \
                  length of words."
         end;
         begin "--set-endeq_len", 
               Arg.Int (fun endeq_len -> 
                   settings := { !settings with endeq_len }),
               ": Settings modification - sets the length of the \
                  'end' of words."
         end;
         begin "--set-mineq_len", 
               Arg.Int (fun mineq_len -> 
                   settings := { !settings with mineq_len }),
               ": Settings modification - sets the length of the \
                  minimum length of words before end is being taken \
                  into consideration."
         end;
         begin "--set-lowercase_convert", 
               Arg.Bool (fun lowercase_convert -> 
                   settings := { !settings with lowercase_convert }),
               ": Settings modification - if true, converts words to \
                  lowercase before comparison."
         end;
       ])

    (fun word -> 
       match !anon_pos with
       | 0 -> incr anon_pos; w0 := word 
       | 1 -> incr anon_pos; w1 := word 
       | _ -> 
         prerr_endline @@ String.concat ""
           [ "[";program_name;"]: Error: You supplied more than two words." ];
         exit 2)

    (String.concat "" 
       [ "\n[";program_name;"] usage:"; 
         " The first two anonymous arguments are compared either with \
            the standard settings or with the manually set settings.\n" ]);

  equal !w0 !w1
    ~verbose:(!verbose)
    ~settings:(!settings)
  |> function
  | true, f ->
    Printf.printf "The words are seen as equal.\nScore: %f\n" f
  | false, f ->
    Printf.printf "The words are NOT seen as equal.\n Score: %f\n" f;
    exit 1




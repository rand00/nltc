(**This module add's 'Batteries Included' functionality to the Ocaml-text module*)
include Text

(**Module inspired by batteries included implementation of enumerable String*)
let enum s =   
  let l = Text.length s in
  let rec make i =
    BatEnum.make
      ~next:(fun () ->
        if !i = l then
          raise BatEnum.No_more_elements
        else
          Text.get s (BatRef.post_incr i)
      )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (BatRef.copy i))
  in
  make (ref 0)

  (**Slower than working with a normal latin-string enumeration, as we don't use mutation*)
let of_enum e = BatList.of_enum e |> BatString.concat ""

let to_stream s = 
  match check s with
  | None -> 
    Stream.from (fun i -> 
      if i >= length s then None 
      else Some (get s i))
  | Some err -> 
    ((print_endline ("\nUnicode-validation error:\n"^err^"\n")); 
     [< >])

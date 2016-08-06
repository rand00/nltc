open Batteries

let all f lst = 
  let rec cmp_with_rest elt acc = function
    | elt'::tl -> cmp_with_rest elt ((f elt elt')::acc) tl
    | [] -> acc
  in  
  let rec cmp_all_rest acc = function
    | elt::tl -> cmp_all_rest ((cmp_with_rest elt [] tl)::acc) tl
    | [] -> acc
  in 
  List.flatten (cmp_all_rest [] lst)
(**[all] is a HOF that takes a function to apply to all
   combinations of elements in the argument list.*)

let all2i compare_fun lst1 lst2 = 
  List.mapi (fun i1 e1 -> 
    i1, List.mapi (fun i2 e2 -> i2, compare_fun e1 e2)
      lst2) 
    lst1
(**[all2i] is a higher order function that takes another
   function and two lists as arg's; and returns a list of the output
   of the arg-function (of two elements) nested with the position of
   the elements in the lists - of each combination of elements of
   list1 and list2.*)
(*goto:omskriv [combine_all2i f] til at give [i] argumenter til [f]
  - på denne måde kan vi undgå at iterere igennem listerne bagefter
  comparisons, og lave alle stats + callback ved comparisons i
  stedet!*)


let factorial i = (1 -- i) |> Enum.reduce ( * )
(**The factorial function - helper for the two following functions*)

let fact_fall x n = 
  (1 -- (n - 1)) 
  |> Enum.fold (fun x_acc m -> 
    x_acc * (x - m) 
  ) x 
(**The falling factorial or falling sequential product*)  

let bincoeff n k = (fact_fall n k) / (factorial k)
(*should the division be done on floats? - if it gives an exact number
  of combinations - therefore an int, it should not be a problem*)
(**The binomial coefficient - counting the number of combinations
   needed for a specific amount of elements.
   @param n is the amount of elements.
   @param k is the length of the sequence of combined elements.*)

let bincoeff_alt n k = ((Int.pow n k) / (factorial k)) - (n / k)
(**My modified binomial coefficient that works?
   - why the fuck did this work!? lucky me.. ;D
   -> must be less efficient than using the falling factorial fun
*)

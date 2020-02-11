
(* 
define a data type for pets, all pets have names (string)
Cats are happy depending on the current temperature (int)
Dogs are always happy

write a function that tells if a pet is happy given the current temperature (int)
*)

 
type pet = 
  | Dog of string
  | Cat of string * (int -> bool)


(* make some example ptes *)
let name (p: pet) : string = 
  match p with
  | Dog s -> s
  | Cat (s, _) -> s
(* write a function that returns when pets are happy at a given temperature, with List.filter and recursion  *)

let isHappy (temp : int) (p : pet) : bool = 
  match p with 
   | Dog _ -> true
   | Cat (_, f) -> f temp
;;
Dog "cs";;
Cat ("cat", fun temp -> temp > 20)

(* let removeUnhappy  *)
let rec removeUnhappy(ls : pet list) (temp : int)  : pet list= 
  match ls with 
  | [] -> []
  | h :: t -> match h with 
              | Dog n -> Dog n :: removeUnhappy t temp
              | Cat (n, f) -> if f temp then Cat (n, f) :: removeUnhappy t temp else removeUnhappy t temp
 
let removeUnhappy (ls : pet list) (temp : int) : pet list =
  List.filter (isHappy temp) ls
(* prove they are equivelent
Hint: 
  check that the functions have the same type
  write a filter that is equivelent to List.filter and 
  write a function that tells if a pet is happy
  *)


(* Currying (if there is time) *)

(*write a function that converts (int -> bool -> string) into  ((int * bool) -> string) *)
(* let curry' (f: (int -> bool -> string)) : ((int * bool) -> string) *)
  
(*write a function that converts ((int * bool) -> string) into (int -> bool -> string)   *)
(* let uncurry' (f: ((int * bool) -> string)) : (int -> bool -> string) *)
  

(* generalize the curry and uncurry functions with polymorphic types *)



(* instertion sort taken from slides*)

(* insert x in to sorted list xs *)
let rec insert' (x : int) (xs : int list) : int list =
  match xs with
   | [] -> [x]
   | hd :: tl ->
    if hd < x 
	  then hd :: insert' x tl
    else x :: xs 

(* polymorphic instertion sort (if there is time) *)
let rec insert (comp : 'a -> 'a -> bool) (x : 'a) (xs : 'a list) : 'a list = 
  match xs with
  | [] -> [x]
  | hd :: tl -> 
    if hd < x
    then hd :: insert comp x tl
    else x :: tl
  
let rec insert_sort'(xs : int list) : int list =
  let rec aux (sorted : int list) (unsorted : int list) : int list =
    match unsorted with
      | [] -> sorted
      | hd :: tl -> aux (insert' hd sorted) tl
  in aux [] xs
(* 
let rec insert  (x : 'a) (xs : 'a list) : 'a list =
*)

(*  
let rec insert_sort (xs : 'a list) : 'a list =
*)
let rec insert (comp : 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
  let rec aux (sorted : 'a list) (unsorted : 'a list) : 'a list =
    match unsorted with 
    | [] -> sorted
    | hd :: tl -> aux (insert compe hd sorted) tl
  in aux [] xs
(* attendance HW questions *)
  
  
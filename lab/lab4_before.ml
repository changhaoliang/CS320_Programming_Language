(* HW solution 1 review *)

(* user defined data *)

(*  warm up: define a data type for streetlights *)
(*
type light = ?
*)
type light  = 
  | Green
  | Red
  | Yellow
  let light1 = Yellow ;;

(* write a function that tells you when to speed up *)
let speedup (l : light) : bool = 
  match l with
  | Yellow -> true
  | Green -> true
  | Red -> false

 (*
let speedup (l:light) : bool = ? 
*)
  

(*
write a datatype representing a student, the student should have a bu-ID, and a year of enrollment
CS students should have a bool (are they taking 320)
Math students have an list of students they are frineds with
*)

type student = 
  | Cs_student of (string * int * bool)
  | Math_student of (string * int * student list)
(*
make some example students
*)
(*
type list' = 
  | Empty
  | Cons of (int, list')*)

let s1 = Cs_student ("7", 2007, true)
let s2 = Math_student ("-1", 3, [])
let s3 = Math_student ("123", 2018, [s1; s2])

(*
write a funcion that gets a student's year of enrollment
*)


let get_year (s:student) : int = 
  match s with
  | Cs_student (_, year, _) -> year
  | Math_student (_, year, _) -> year


(*
write a function that tells if a student is in 320 or has a friend connection with a 320 student
*)
let rec knows320ls(ls : student list) : bool = 
  match ls with 
  | []
  | (Cs_student (_, _, true)) :: _ -> true
  | (Cs_student (_, _, false)) :: xs -> knows320ls xs
  | (Math_student (_, _, xs')) :: xs -> knows320ls xs' || knows320ls xs

let rec knows320 (s:student) : bool = 
  match s with
  | Cs_student (_, _, in320) -> in320
  | Math_student (_, _, xs) -> knows320ls xs

(*
(is it possible for a student to be friends with themselves?)
*)

(* time permitting :
make your own custom data types that match the standard ones:
bool, list of int
*)


(* time permitting: 
define a data type for pets, all pets have names (string)
Cats are happy depending on the current temperature (int)
Dogs are always happy

write a function that tells if a pet is happy given the current temperature (int)
*)


(* higher order functions , anonomous functions *)


(* use List.map to turn a list of students into a list of cs students  *)
(*let transfer (ls : student list) : student list = 
  match ls with
  | [] -> []
  | (Math_student (id, year, _)) :: xs -> (Cs_student (id, year, false)) :: (transfer xs)
  | x :: xs -> x :: (transfer xs)*)

let transfer = List.map
  (fun s -> match s with
  | (Math_student (id, year, _))  -> (Cs_student (id, year, false))
  |  x -> x) 
(* use List.filter to select all the students with a year < 2019 *)


(* time permitting: Currying *)

(*write a function that converts (int -> bool -> string) into  ((int * bool) -> string) *)
(*
let curry' (f: (int -> bool -> string)) : ((int * bool) -> string) = ?
*)
  
(*write a function that converts ((int * bool) -> string) into (int -> bool -> string)   *)
(*
let uncurry' (f: ((int * bool) -> string)) : (int -> bool -> string) = ?
*)
  


(* time permitting: polymorphism *)

(* generalize the curry and uncurry functions *)

(* time permitting: 
make your own custom data types that match the standard ones:
bool, option, list, 
*)


  
(* issues with attendence, will not collect attendence today *)


(* HW solution 1 review *)


(*
how do you convert a loop to function recursion

- everything variable that changes becomes an argument
- all the information used after the loop will make the return type
- start the function with the initial values
- check if you continue to loop with an if statement
  - if you are done return the outputs
  - if you are not done increment every variable in a recursive call

for example this python:

x = 1
y = 2
while x < 11:
    x = x + 2
    y = y + 4

print(x, y)

would become this ocaml
*)
let rec simpulateLoop (x: int) (y: int) : (int * int) =
  if x < 11
  then simpulateLoop (x + 2 ) ( y + 4)
  else (x, y)
in simpulateLoop 1 2
;;


(* user defined data *)


(*  warm up: define a data type for streetlights (lab 2 took this in a different direction) *)

type streetlight = 
  | On 
  | Off

let l1 : streetlight = On
let l2 : streetlight = Off


(*  should you walk down the street? *)
let walkdown (l:streetlight) : bool = 
  match l with
   | On -> true
   | Off -> false



(*  warm up: define a data type for traffic lights *)

type light = 
  | Red 
  | Yellow
  | Green

let l1 =  Red
let l2 =  Yellow
let l3 =  Green

(* write a function that tells you when to speed up *)
let speedup (l:light) : bool = 
  match l with
    | Green  -> true
	| Yellow -> true
	| Red    -> false
  

(*
write a datatype representing a student, the student should have a bu-ID, and a year of enrollment
CS students should have a bool (are they taking 320)
Math students have an list of students they are frineds with
*)

type student = 
  | Cs_Student of (string * int * bool)
  | Math_Student of  (string * int * (student list) )

(*
make some example students
*)

let s1 = Cs_Student ("7", 2007, true)
let s2 = Math_Student ("-1", 3, [])
let s3 = Math_Student ("123", 2018, [s1 ; s2])

let s4 = Cs_Student ("123", 2020, false)
let s5 = Math_Student ("234", 2016, [s4; s4])
let s6 = Math_Student ("567", 2016, [s4; s5;  Cs_Student ("123", 2018, true)])

let s7 = Cs_Student ("0",1,true)
let s8 = Math_Student ("1", 199, [])
let s9 = Math_Student ("2",2019, [s7; s8])

let s10 = Cs_Student ("Jon", 2015, false)
let s11 = Cs_Student ("007" , 2007, true)
let s12 = Math_Student ("123", 2016, s10 :: s11 :: [])


let sls = [s1 ; s2 ; s3]




(*
write a funcion that gets a student's year of enrollment
*)

let get_year (s:student) : int = 
  match s with
    | Cs_Student (_, year, _) -> year
	| Math_Student (_, year, _) -> year



(*
write a function that tells if a student is in 320 or has a friend connection with a 320 student
*)

(* lab 1 solution *)
let rec knows320ls (ls: student list) : bool = 
  match ls with
  | [] -> false
  | (Cs_Student (_, _, true)) :: _ ->  true
  | (Cs_Student (_, _, false)) :: xs ->  knows320ls xs
  | (Math_Student (_, _, xs')) :: xs -> knows320ls xs' || knows320ls xs

let rec knows320 (s:student) : bool = 
  match s with
  | Cs_Student (_, _, in320) -> in320
  | Math_Student (_, _, xs) -> knows320ls xs


(* lab 2+3 solution *)
let rec orAll (ls : bool list) : bool =
  match ls with
    | [] -> false
	| x :: xs -> x || orAll xs

let rec knows320 (s:student) : bool = 
  match s with
    | Cs_Student (_, _, in320) -> in320
    | Math_Student (_, _, frineds) ->  orAll (List.map knows320 frineds)
	
(*remember List.map is defined as *)
let rec map' f ls =
  match ls with
    | []      -> []
	| x :: xs -> (f x) :: map' f xs
	
(* lab 4 solution *)

(* from lecture slides *)
let rec reduce (f:student -> bool -> bool) (b : bool) (xs:student list) : bool =
  match xs with
    | [] -> b
    | hd::tl -> f hd (reduce f b tl) 

let rec knows320 (s:student) : bool = 
  match s with
    | Cs_Student (_, _, in320) -> in320
    | Math_Student (_, _, friends) -> reduce (fun s b -> knows320 s || b ) false friends

	
(* solution  with List.fold_left *)
	
let rec knows320 (s:student) : bool =
  match s with
    | Cs_Student (_ , _ , b) -> b
    | Math_Student (_ , _ , ls) -> List.fold_left (fun acc s -> acc || knows320 s) true ls


(*
(is it possible for a student to be friends with themselves?)
*)

(*
the short answer is no, you should think of constructors building up trees where every sub tree is smaller than the full tree.
  most functions work on this assumption, it is how we know all the above functions terminate
*)
(*
Technically ocaml has a quirck that lets cyclic values be defined
let rec st4 = Math_Student ("self", 207, st4 :: [])
we will not use this trick in this class
*)



(* higher order functions , anonomous functions *)


(* use List.map to turn a list of students into a list of cs students  *)

let tocs (s:student) : student = 
  match s with
   | Math_Student (id, year, _ ) -> Cs_Student (id, year, false)
   | x -> x

let transfer' (ls : student list) : student list = List.map tocs ls
let transfer : student list ->  student list  = List.map tocs


(* use List.filter to select all the students with a year < 2019 *)

let oldstudents' (ls: student list) : student list = List.filter (fun s -> get_year s < 2019) ls 
let oldstudents : student list -> student list = List.filter (fun s -> get_year s < 2019) 


(* question from lab 1 is there a data type with no constructors? *)



(* lab4_before.ml contains more practice problems the wwern't covered in any of the labs *)

  
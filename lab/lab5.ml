
(* 
define a data type for pets, all pets have names (string)
Cats are happy depending on the current temperature (int)
Dogs are always happy

write a function that tells if a pet is happy given the current temperature (int)
*)


type pet = 
  | Dog of string
  | Cat of string * (int -> bool)


(* make some example pets *)
let cs = Dog "cs"
let cat = Cat ("cat", fun temp -> temp > 20)

let tabby = Cat ("tabby", fun _ -> false)
let oscar = Cat ("oscar", fun temp -> temp > 95)

let anything = Dog "anything"
let cheeseburger = Dog "cheeseburger"
let hotdog = Cat ("hotdog", fun _ -> false)
let suasage = Cat ("suasage", fun temp -> temp mod 2 == 0)

let buddy = Dog "buddy"
let tom = Cat ("Tom", fun temp -> temp == 72)
let bob =  Cat("Bob", fun temp -> 65 <= temp && temp <= 80)

let pets = [cs;cat;tabby;oscar;anything;cheeseburger;hotdog;suasage;buddy;tom;bob]


(* we can be sure that our type works for the specification by writing functions that extract the properties we want from the types *)
let name (p:pet) : string = 
  match p with
  | Dog s -> s
  | Cat (s,_) -> s

(* temperature*)
let isHappy (temp :int) (p:pet) : bool = 
  match p with
  | Dog _ -> true
  | Cat (_,f) -> f temp


(* write a function that returns when pets are happy at a given temperature *)

let rec removeUnhappy (temp : int) (ls : pet list) : pet list =  
  match ls with
    | [] -> []
    | h :: tail -> (match h with 
	            | Dog n ->  h :: (removeUnhappy temp tail)  
	            | Cat (n, f) -> if f temp 
				                then h :: removeUnhappy temp tail
								else removeUnhappy temp tail)
(* or more simply *)		
let rec removeUnhappy (temp :int)  (ls:pet list) : pet list =
  match ls with
  | [] -> []
  | h :: t -> if isHappy temp h
              then h :: removeUnhappy temp t
			  else removeUnhappy temp t

(* or more simply *)
let removeUnhappy (temp :int)  (ls:pet list) : pet list  = List.filter (isHappy temp) ls

(* or more simply *)	
let removeUnhappy (temp :int)  = List.filter (isHappy temp)



(* prove the recursive definition is operationally equivelent to the List.filter definition *)

(* first check that the functions have the same type:
   they do
   int -> pet list -> pet list
  *)

(* List.filter is operationally equivelent to the following *)
let rec filter (f : 'a -> bool) (ls : 'a list) : 'a list =
  match ls with
  | [] -> []
  | head :: tail -> if f head 
                    then head :: filter f tail 
					else filter f tail


let rec removeUnhappy       (temp:int)  = List.filter (isHappy temp)
(* =                                                                   since List.filter = filter *)
let rec removeUnhappy       (temp:int)  =      filter (isHappy temp)   
(* =                                                                   desugar *)
let rec removeUnhappy = fun (temp:int) ->      filter (isHappy temp)
(* =                                                                   eta expand *)
let rec removeUnhappy = fun (temp:int) ->  fun (ls : pet list) ->  
  filter (isHappy temp) ls
(* =                                                                   apply def of filter *)
let rec removeUnhappy = fun (temp:int) ->  fun (ls : pet list) ->  
  filter (isHappy temp) ls
(* =                                                                   apply def of filter with f = (isHappy temp) and ls = ls *)
let rec removeUnhappy = fun (temp:int) ->  fun (ls : pet list) ->  
  match ls with
    | [] -> []
    | head :: tail -> if (isHappy temp) head 
	                  then head :: filter (isHappy temp) tail 
				      else filter (isHappy temp) tail
(* =                                                                   rewrite filter (isHappy temp) = (removeUnhappy temp)  which is given by the 2nd equation *)
let rec removeUnhappy = fun (temp:int) ->  fun (ls : pet list) ->  
  match ls with
    | [] -> []
    | head :: tail -> if  isHappy temp  head 
	                  then head :: (removeUnhappy temp) tail 
				      else (removeUnhappy temp) tail
(* =                                                                   resugar *)
let rec removeUnhappy (temp:int) (ls : pet list) =
  match ls with
    | [] -> []
    | head :: tail -> if  isHappy temp  head 
	                  then head ::  removeUnhappy temp  tail 
				      else  removeUnhappy temp  tail       


(* tophat question:
if map is defined as
*)
let rec map f xs =
match xs with
| [] -> []
| hd::tl -> (f hd)::(map f tl) ;;
(* what is *)
fun ys -> map (fun x -> x + 1) (2 :: ys)  ;;
(* equivalent to? *)


(*A*) fun xs -> match xs with | [] -> [] | hd::tl -> (hd + 1)::(map (fun x -> x + 1) tl) ;;

(*B*) fun ys -> 3 :: map (fun x -> x + 1) (ys) ;;

(*C ) 3 :: map (fun x -> x + 1) *)

(* 
first typecheck 
*)
((fun ys -> map (fun x -> x + 1) (2 :: ys))                                                 : int list -> int list) ;;
(*A*) ((fun xs -> match xs with | [] -> [] | hd::tl -> (hd + 1)::(map (fun x -> x + 1) tl)) : int list -> int list) ;;
(*B*) ((fun ys -> 3 :: map (fun x -> x + 1) (ys))                                           : int list -> int list) ;;

(*C does not typecheck since, 
map (fun x -> x + 1) needs to have the type "int list"
but it has type
*)
(map (fun x -> x + 1) : int list -> int list) ;;

(*A  is not opertationally equivelent since they give different arguments at the same results *)
(fun ys -> map (fun x -> x + 1) (2 :: ys))                                           [] ;; (* = [3] *)
(fun xs -> match xs with | [] -> [] | hd::tl -> (hd + 1)::(map (fun x -> x + 1) tl)) [] ;; (* = [] *)

(*B  is not opertationally equivelent with the following proof *)
fun ys -> map (fun x -> x + 1) (2 :: ys) ;;
(* =                                                                   expand map with f = (fun x -> x + 1) and ls = (2 :: ys) *)
fun ys -> match (2 :: ys) with
          | [] -> []
          | hd::tl -> ((fun x -> x + 1) hd)::(map (fun x -> x + 1) tl) ;;
(* =                                                                   match the value hd = 3, tl = ys *)
fun ys -> ((fun x -> x + 1) 2)::(map (fun x -> x + 1) ys) ;;
(* =                                                                   eval (fun x -> x + 1) at 2 *)
fun ys -> (          2 + 1   )::(map (fun x -> x + 1) ys) ;;
(* =                                                                   2 + 1 = 3   *)
fun ys ->              3      :: map (fun x -> x + 1) ys  ;;



(* instertion sort taken from slides*)

(* insert x in to sorted list xs *)
let rec insert' (x : int) (xs : int list) : int list =
  match xs with
   | [] -> [x]
   | hd :: tl ->
     if hd < x 
	 then hd :: insert' x tl
     else x :: xs 


	 
let insert_sort' (xs : int list) : int list =
  let rec aux (sorted : int list) (unsorted : int list) : int list =
    match unsorted with
      | [] -> sorted
      | hd :: tl -> aux (insert' hd sorted) tl
  in aux [] xs
  

(* a polymorphic version *)
let rec insert (lt: 'a -> 'a -> bool) (x : 'a) (xs : 'a list) : 'a list =
  match xs with
   | [] -> [x]
   | hd :: tl ->
     if lt hd x 
	 then hd :: insert lt x tl
     else x :: xs 
  
	 
let insert_sort  (lt: 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
  let rec aux (sorted : 'a list) (unsorted : 'a list) : 'a list =
    match unsorted with
      | [] -> sorted
      | hd :: tl -> aux (insert lt hd sorted) tl
  in aux [] xs

;;
(* this give more flexibility even with lists of nat *)
insert_sort (>) [1; 4; 2; 10];;
insert_sort (<) [1; 4; 2; 10];;



(* formatting hint that came up with HW questions *)

(* formatting hint *)

(* you want to do something like

let f (op : pet -> int option) (x :pet) (y : pet) : int option =
   op x + op y
   
but this doesn't type check,
op x  : int option
op y  : int option
_ + _ : int

this can be fixed by matching op x and op y
*)
let f (op : pet -> int option) (x :pet) (y : pet) : int option =
   match op x with
   | None    -> op y
   | Some x' -> match op y with
                | None    -> Some x'
                | Some y' -> Some (x' + y')
				
(* you can do this more readably by tupling the pattern match! *)		
let f (op : pet -> int option) (x :pet) (y : pet) : int option  =
   match (op x, op y) with
    | (None   , None   ) -> None
    | (None   , Some y') -> Some y'
    | (Some x', None   ) -> Some x'
    | (Some x', Some y') -> Some (x' + y')
  
(*
this week we will use a seperate file called lab2.ml
we will allways check our work by running an interperter side by side

#use "lab2.ml";;
will reload the file after we make changes
*)

(*
note I made some small mistakes in my presentation
I sometimes used == instead of the more correct check =.  == can sometimes lead to unexpected results for instance 1.0 == 1.0
I used "Char.escaped" instead of "String.make 1".  Char.escaped would have had unexpected results on special characters like new lines.

The notes have been corrected to show better usage
*)


(*
function review
*)

(*
define an or function
*)

let or1 (x : bool) (y : bool) : bool = 
  if x = false 
  then (if y = false 
        then false
		else true)
  else true ;;

  
(* alternatively *)
  
let or2 (a:bool) (b:bool) : bool =
  if a =  true
  then true
  else if b = true
       then true
	   else false;;

(* which can be better writen as *)
	   
let or3 (a:bool) (b:bool) : bool =
  if a
  then true
  else if b
       then true
	   else false;;

(* which can be better writen as *)

let or4 (a:bool) (b:bool) : bool =
  if a
  then true
  else b;;


(*
write a multiplication function in terms of + (for non-negative inegers)
*)

let rec mult (a : int) (b : int) : int =
  if a = 0
  then 0
  else b + mult (a - 1) b
;;

(* you can also recurse on the 2nd paraneter *)

let rec mult' (a: int) (n:int) :int =
  if n = 0
  then 0
  else a + mult a (n - 1)
;;


(*
what does "mult 3" do?
*)


(* this will create a new function that multiplies by 3 *)
let myst = mult' 3 ;;

(* you can think of it as replacing the a with 3, but leaving n as a parameter *)
let rec mult'3  (n:int) :int =
  if n = 0
  then 0
  else 3 + mult'3 (n - 1)
;;

(*
do the types make sense?
*)

(*
yes.
mult : int -> int -> int
which is
mult : int -> (int -> int)
so if mult is given a single int we get back a function of type int -> int
mult 3 : int -> int
*)



(*
how can we "loop" with recursion?

write a function to add numbers between a range
*)

(*
in python this could be done as

def sum(start, finish):
  counter = 0
  for i in range(start, finish + 1) :
      counter = counter + i
  return counter

OR
  
def sum(start, finish):
    counter = 0
    i = start
    while i < finish + 1:
        counter = counter + i
        i = i + 1
    return counter

*)

(* this can be translated directly *)
let sum (start :int) (finish :int) :int = 
  let rec loop (i: int) (counter :int) : int = 
    if i > finish
	then counter
	else loop (i + 1) (counter + i)
  in loop start 0 
;;


(* we don't need to track the counter directly, and can modify the putput of the function *)
let sum' (start:int) (finish:int) :int =
    let rec loop (i : int) : int =
      if i < finish
      then i + loop (i + 1)
      else i
    in loop start;;


(* or we can write it in a more direct style *)
let rec sum'' (start :int) (finish :int) : int =
  if start < finish
  then start + sum (start + 1) finish
  else finish
;;


(*
tuples, how do the types work? what are the values?
*)

(* string * string *)
("hi", "bye") ;;

(* int * string * bool *)
(5, "hello", false) ;;

(* (int -> int -> int) * string * int *)
(mult, "hi", 10) ;;


(*
define an or function that pattern matches on tuples
*)

let or5 (a:bool) (b:bool) : bool =
  match (a, b) with            (* create a tuple of type bool * bool for pattern matching *)
      (true  , true ) -> true
	| (true  , false) -> true
	| (false , true ) -> true
	| (false , false) -> false
;;

let or6 (a:bool) (b:bool) : bool =
  match (a, b) with
      (false , false)  -> false
	| (_     , _    )  -> true
;;


(*
string operations (using the interperter to explore, what are the types? what do they do?)
*)

String.length;;
String.contains;;
String.get;;


(*
write a function that reverses strings
*)
(*
in python you could write the function as

def rev(str):
    temp = ""
    for i in range(len(str) - 1, -1, -1):
        temp = temp + str[i]
    return temp

OR
	
def rev(str):
    temp = ""
    i = len(str) - 1
    while i >= 0 :
        temp = temp + str[i]
        i = i - 1
    return temp

*)


let rev (str: string) : string = 
  let rec loop (i:int) (temp:string) : string = 
    if i < 0
	then temp
	else loop (i - 1) (temp ^ ( String.make 1  (String.get str i)))
  in loop (String.length str - 1) "" 
;;




(*
finally attendence
*)
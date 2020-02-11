

(* tomphat questions (not every lab answered every question)*)
(*
what type is exactly the same as
int -> string option option -> char * bool
?
	
int -> ((string option) option -> (char * bool))
(int -> string (option option)) -> (char * bool)
(int -> (string option) option) -> (char * bool)
int -> (string (option option) -> char) * bool

the answer is
int -> ((string option) option -> (char * bool))
*)
(*
Consider the following pattern matching expression:
match x with
| (None,  0) -> "a"
| (Some true, 0) -> "b"
| (Some false, n) -> "c"
What is the type of x?

the answer is
(bool * int) option
*)

(*
What is the type of x in the following expression?
match x with
| (p,q) -> match q with  | 0 -> (1,1)
                         | n -> match p with 
                                | (true, false) -> (1,2)
                                | (false, true) -> (4,2)
                                | _ -> (5, 9)
								

the answer is
(bool * bool) * int
*)

(*
Which one of the following values of x would cause the following expression to evaluate to 3?
match x with
| (0, p, q) -> 1
| (r, p, 1) ->  2
| (_,_, _)  -> 3

a possible answer is
(5, 1, 0)
*)

(* basic pattern matchimg *)

match 10 with
   | 9 -> "hi"
(* | Some 9 -> "bye" *)  (* pattern matchs only work when the types agree *)
   |  _ -> "?"
;;



match (0,1,1) with
 | (0, p, q) -> 1 
 | (r, p, 1) -> 2  
 | _ -> 3
(* evaluates to 1, the first matching branch is taken *)
;;


match (0,1,1) with
 | (0, p, q) -> p + (q *10000)
 | (r, p, 1) -> 2
 | _ -> 3
 
(* evaluates to 10001 *)
;;


match (0,2,4) with
 | (0, _, _) -> 1
 | (_, _, 1) -> 2  
 | _ -> 3
 
(* when variables are not used it is good to write _ *)
;;



(* some example option values *)

(Some 0 : int option);;
(Some 5 : int option);;
(Some 7 : int option);;
(None   : int option);;

(Some "hi"     : string option);;
(Some "string" : string option);;
(Some "hello"  : string option);;
(None          : string option);;

(Some true : bool option );;

(Some (5 , 8) : (int * int) option) ;;

(Some (Some 3)   : int option option );;

(Some (Some (Some true), 8 ) : (bool option option * int) option );;


(* more complicated expressions *)
(Some (let f (x : int) : int = x + x in f)
    : (int -> int) option)
;;
(Some (Some (let f (x : (int * bool) option) : (string option) = None in f))
    : ( (int * bool) option -> (string option)) option option)
;;

(* every option value is
     None
   or
     Some _
*)


(* imagine you are writing a function for a new version of BU's student link,
   the function must take in year names "freshman", "sophomore", ...
   and provide how many years the student has completed (freshmen have completed 0 years)
   what should the type of the function be? 
   string -> int option 
   write that function
   *)

let year (s : string) : int option = 
  match s with
    | "freshman"  -> Some 0
	| "sophomore" -> Some 1
	| "junior"    -> Some 2
	| "senior"    -> Some 3
	| _           -> None
;;
   


(* some example list values  *)

([3 ;  4     ]     : int list);; (* same as *)
(3 ::  4 :: []     : int list);; (* same as *)
(3 :: (4 :: [])    : int list);;

([4]               : int list);; (* same as *)
(4 :: []           : int list);;

(5 :: []           : int list);;
(10 :: 5  :: []    : int list);;
(5 :: 6 :: []      : int list);;
(1 :: 2 :: 3 :: [] : int list);;
( []               : int list) ;;


((true, false) :: (true, true) :: []  : (bool * bool) list ) ;;


(  (4 :: (5 :: [])) :: []     : int list list);;
(  ( 5 :: 3 :: (-4) :: []) 
:: (6 :: []) 
:: []                         : int list list  );;


( ( ( None  :: Some 2 ::  [] ) :: []) : int option list list);;


(* more complicated expression *)
((let f x = x + x in f) :: [] : (int -> int) list ) ;;

(* every list value is
     []
   or
     _ :: _
*)


(* what would be a good type for a function that gets the last element from a list (of ints)? *)
(*   int list -> int option *)
(* write that function *)

let rec last (ls : int list) : int option =
  match ls with
  | [] -> None
  | x :: xs -> (
     match xs with
      | [] -> Some x (* return Some x*)
      | _ -> last xs
    ) 
;;
let rec last' (ls : int list) : int option =
  match ls with
  | []      -> None
  | x :: [] -> Some x
  | x :: xs -> last' xs
  
;;

(* function that indexes into a list *)

let rec get (ls : int list ) (i : int) : int option  =
  match ls with
    | [] -> None
  	| (x :: xs ) -> if i = 0 then Some x
					          else get xs (i - 1)
;;


(* effects via let expressions *)

(* write a function that combines the input of in1.txt and in2.txt and writes that into file out.txt *)


(* HINT: if you have trouble try first solving simpler related problems:
   can you get the strings from a file?
   can you write strings to a file?   *)

(* read a single line from a file *)
let openfile' (_ : unit) : string =
  let ch = open_in "file.txt" in (* ch = in_channel type*)
  let line = input_line ch in
  let _ = close_in ch in
  line
;;

(*
call it like:
openfile' () ;;
*)

(* read a all the lines from a file *)
let rec readlines (ch : in_channel) : string list =
  match input_line ch with 
    | str -> str :: readlines ch
    | exception  End_of_file -> [] (* input_line throws an exception at the end of the file *)
;;


let openfile (_ : unit) : string =
  let ch = open_in "file.txt" in
  let lines = readlines ch in
  let _ = close_in ch in
  String.concat "..." lines
;;

(* write a single line to a file  *)
let writef' (_ : unit) : unit =
  let ch = open_out "out.txt" in
  let _  = Printf.fprintf ch "%s\n" "hi" in
  let _ = close_out ch in (* close input file in_channel -> unit*)
  () (*retun ()*)
;;

(* write many lines to a file *)
let rec write (ch) (ls : string list ) : unit =
  match ls with
    | [] -> ()
    | x :: xs -> let _ = Printf.fprintf ch "%s\n" x in write ch xs
;;
(* write into a given file  out.txt*)
let writef (_ : unit) : unit =
  let ch = open_out "out.txt" in (* open the file, if not exist, create one*)
  let _  = write ch ("hello world" :: "good job lab 4" :: []) in (* write line into it*)
  let _ = close_out ch in (* close the file*)
  ()
;;

(* one way to put it all together  *)
let appendFiles  (() : unit) : unit =

  let ch = open_in "in1.txt" in
  let lines1 = readlines ch in
  let _ = close_in ch in
  
  
  let ch = open_in "in2.txt" in
  let lines2 = readlines ch in
  let _ = close_in ch in
  
  (* in1 ^ in2 -> in3 *)
  
  let ch = open_out "out.txt" in
  let _  = write ch (List.append lines1 lines2) in
  let _ = close_out ch in
  
  ()


  

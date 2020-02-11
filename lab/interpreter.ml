
(* things that goes on a stack *)
type stackVal = 
    I of int 
  | S of string 
  | N of string
  | B of bool 
  | U
  | E 

(* well formed instructions *)
type command = PushI of stackVal 
             | PushS of stackVal 
             | PushN of stackVal 
             | PushB of stackVal
             | Push of stackVal
             | Add | Sub | Mul | Div | Rem | Neg
			 | Pop
			 | Swap
			 | Quit 


let rec run (commands : command list) (stack: stackVal list) : stackVal list = 
  match (commands , stack)  with
  | (PushI (I i) :: rest, _              ) -> run rest (I i :: stack)
  | (PushI _     :: rest, _              ) -> run rest (E :: stack)
  
  | (Add         :: rest, I i :: I j ::s') -> run rest (I (i+j) :: s')
  | (Add         :: rest, _              ) -> run rest (E  :: stack)
  
  | (Sub         :: rest, I i :: I j ::s') -> run rest (I (i-j) :: s')
  | (Sub         :: rest, _              ) -> run rest (E :: stack)
  
  | (Mul         :: rest, I i :: I j ::s') -> run rest (I (i*j) :: s')
  | (Mul         :: rest, _              ) -> run rest (E :: stack)
  
  | (Div         :: rest, I i :: I 0 ::s') -> run rest (E :: stack)
  | (Div         :: rest, I i :: I j ::s') -> run rest (I (i/j) :: s')
  | (Div         :: rest, _              ) -> run rest (E :: stack)
  
  | (Rem         :: rest, I i :: I 0 ::s') -> run rest (E :: stack)
  | (Rem         :: rest, I i :: I j ::s') -> run rest (I (i mod j) :: s')
  | (Rem         :: rest, _              ) -> run rest (E :: stack)
  
  | (Neg         :: rest, I i ::s'       ) -> run rest (I (-i) :: s')
  | (Neg         :: rest, _              ) -> run rest (E :: stack)
  
  | (PushS (S s) :: rest, _              ) -> run rest (S s :: stack)
  | (PushS _     :: rest, _              ) -> run rest (E :: stack)
  
  | (PushN (N n) :: rest, _              ) -> run rest (N n :: stack)
  | (PushN _     :: rest, _              ) -> run rest (E :: stack)
  
  | (PushB (B b) :: rest, _              ) -> run rest (B b :: stack)
  | (PushB _     :: rest, _              ) -> run rest (E :: stack)
  
  | (Push U      :: rest, _              ) -> run rest (U :: stack)
  | (Push E      :: rest, _              ) -> run rest (E :: stack)
  | (Push _      :: rest, _              ) -> run rest (E :: stack)
  
  | (Pop         :: rest, _ ::s'         ) -> run rest s'
  | (Pop         :: rest, []             ) -> run rest (E  :: [])
  
  | (Swap        :: rest, x::y::s'       ) -> run rest (y::x::s')
  | (Swap        :: rest, _              ) -> run rest (E :: stack)
  
  | (Quit        :: _   , _              ) -> stack
  | ([]                 , _              ) -> stack

(* remember to test! *)
let e1 = run [PushI (I 1); PushI (N "2.5"); PushI (N "x")] []
(* ... *)


(* writing *)
let to_string (s : stackVal) : string = 
  match s with
  | I i -> string_of_int i 
  | S s  -> s
  | N n -> n
  | B b -> "<" ^ string_of_bool b ^ ">"
  | U   -> "<unit>"
  | E   -> "<error>"

(* parsing *)
(* helper functions *)
let explode (s:string) : char list =
  let rec expl i l =
    if i < 0 
	then l 
	else expl (i - 1) (String.get s i :: l)
  in expl (String.length s - 1) []

let implode (cl:char list) : string = 
  String.concat "" (List.map (String.make 1) cl)

 
let is_alpha (c:char): bool = 
  (Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z')
  || (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let is_digit (c:char): bool = 
 Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let rec take_while' (p:char -> bool) (es : char list) : (char list) * (char list) = 
  match es with
  | []      -> ([],[])
  | x :: xs -> if p x then let (chars, rest) = take_while' p xs in  (x :: chars, rest) else ([],xs)

let take_while (p:char -> bool) (s:string) : string * string = 
  let (echars, erest) = take_while' p (explode s) 
  in (implode echars, implode erest)


let parse_int (s : string) : int option = 
	match int_of_string s with	
	| n -> Some n
	| exception _ -> None

let parse_string (s : string) : string option = 
	if String.length s > 1 && String.get s 0 = '"' && String.get s (String.length s - 1) = '"'
	then  Some (String.sub s 1 (String.length s - 2)) (* this is less restrictive then the spec *)
	else None


let parse_name (s : string) : string option = 
	if String.length s > 0 && ( let c = (String.get s 0) in is_alpha c ||  c = '_')
	then  Some s (* this is less restrictive then the spec *)
	else None
	
let parse_constant (s:string) : stackVal = 
    let s' = String.trim s in
	match s' with
	| "<true>"  -> B true
	| "<false>" -> B false
	| "<unit>"  -> U
	| _ -> match parse_int s' with
	       | Some i -> I i
	       | None -> match parse_string s' with
	                 | Some s -> S s
	                 | None -> match parse_name s' with
	                           | Some s -> N s
	                           | None -> E



let parse_command (s:string) : command = 
	match take_while is_alpha (String.trim s) with
	| ("PushI", p) -> PushI (parse_constant p)
	| ("PushS", p) -> PushS (parse_constant p)
	| ("PushN", p) -> PushN (parse_constant p)
	| ("PushB", p) -> PushB (parse_constant p)
	| ("Push" , p) -> Push (parse_constant p)
	| ("Add"  , _) -> Add
	| ("Sub"  , _) -> Sub
	| ("Mul"  , _) -> Mul
	| ("Div"  , _) -> Div
	| ("Rem"  , _) -> Rem
	| ("Neg"  , _) -> Neg
	| ("Pop"  , _) -> Pop
	| ("Swap" , _) -> Swap
	| ("Quit" , _) -> Quit
	(* any unknown commands will result in an exception *)

	

(* file IO *)

(* from lab 3 *)
let rec read_lines (ch : in_channel) : string list =
  match input_line ch with 
    | str                    -> str :: read_lines ch
    | exception  End_of_file -> [] (* input_line throws an exception at the end of the file *)
	
(* from lab 3 *)
let rec write_lines (ch) (ls : string list ) : unit =
  match ls with
    | []      -> ()
	| x :: xs -> let _ = Printf.fprintf ch "%s\n" x in write_lines ch xs
	

(* run the interperter on the commands in inputFile and write the resulting stack in outputFile *)
let interpreter (inputFile : string) (outputFile : string) : unit =
  let ic = open_in inputFile in
  let lines_in = read_lines ic in
  let _ = close_in ic in
  
  let commands = List.map parse_command lines_in in
  let stack = run commands []  in
  let lines_out = List.map to_string stack in
  
  let oc = open_out outputFile in
  let _ = write_lines oc lines_out in
  let _ = close_out oc in ()
 
  
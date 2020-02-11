
(* Attendence *)

(* Part 1: reasoning with types *)

type exp = Add of exp * exp | Sub of exp * exp | I of int | Name of string

(* some examples *)
let e1 = I 3
let e2 = Add (I 3, I 2)
let e3 = Add (Sub (I 3, I 5), Sub (I 3, I 5))
let e4 = Add (I 1, Name "x")

(* task: some names can be bound in expressoions, write a function that evaluates the expresion with the given type signature *)
(*
let eval (e:exp) : int =  ???
*)
(* trick question.  it is "impossible" to write an evaluation function at that type signature. There is not enough infomation *)


(* can modify the function to track the an environment encoded with the type (string * int) list.  other encodings are possible *)

(* some examples *)
let e11 = [("x" , 3), ("y"  , 4)]
let e12 =  [("x",6)]

let rec fetch (name :string)  (env: (string * int) list) : int option = 
    match env with
	  | (name' , v) :: rest -> if name = name' then Some v else fetch name rest
	  | [] -> None
  
let rec eval (e:exp) (env: (string * int) list) : int option  =
  match e with
    | I i -> Some i
	| Name n -> fetch n env
	| Add (x,y) -> (match (eval x env, eval y env ) with (Some x' , Some y') -> Some (x' + y') | _ -> None)
	| Sub (x,y) -> (match (eval x env, eval y env ) with (Some x' , Some y') -> Some (x' - y') | _ -> None)
	
let t1 = eval (Add (I 1, Name "x")) [("y", 7) ;("x", 5)]
let t2 = eval (Add (I 1, Name "x")) (("x", 1):: [("y", 7) ;("x", 5)])
	

(* test on some examples *)



(* Part 2 : extending the robot example from lab 7 *)


(* (some typeo corrections)

rules in the form 
(p,E,m) → (p’,E’,m’)
p is a program
E is an environment
m is a position

i deontes an integer,
x, x' denote variables



-----------------------------------------------
(Step Right :: rest, E, n) -> (rest, E, n + 1)


-----------------------------------------------
(Step Left :: rest, E, n) -> (rest, E, n - 1)


-----------------------------------------------
(Jump Right i :: rest, E, n) -> (rest, E, n + i)


                   x in E
-----------------------------------------------
(Jump Right x :: rest, E, n) -> (rest, E, n + fetch(x,E))



-----------------------------------------------
(Jump Left i :: rest, E, n) -> (rest, E, n - i)


                   x in E
-----------------------------------------------
(Jump Left x :: rest, E, n) -> (rest, E, n - fetch(x,E))


-----------------------------------------------------
(Set x i :: rest , E, n) -> (rest, x' = i in E, n)


                                x in E
-------------------------------------------------------------
(Set x' x :: rest , S, E) -> (rest, x' = fetch(x,E) in E, n)

 *)

type dir = Left | Right

type dist = I of int | Var of string

type inst = Step of dir | Jump of dir * dist | Set of string * dist 


let rec runRobot (intructions : inst list) (pos:int) (env: (string * int) list ) : int option =
  match intructions with
  
	| Set (s, I i):: rest -> runRobot rest pos ((s, i) :: env)
	| Set (s, Var n):: rest -> (match fetch n env with Some i -> runRobot rest pos ((s, i) :: env) | _ -> None )
	
    | Step Left :: rest -> runRobot rest (pos - 1) env
    | Step Right :: rest -> runRobot rest (pos + 1) env

    | Jump (Left , I i) :: rest -> runRobot rest (pos - i) env
    | Jump (Right , I i) :: rest -> runRobot rest (pos + i) env
    | Jump (Right , Var s) :: rest -> (match fetch s env with Some i -> runRobot rest (pos + i) env | _ -> None )
    | Jump (Left , Var s) :: rest -> (match fetch s env with Some i -> runRobot rest (pos - i) env | _ -> None )

	| [] -> Some pos

(* helper functions can make the code more readable *)
let fetch' (d: dist)  (env : (string * int) list )  : int option =
  match  d with
    | I i -> Some i
	| Var v -> fetch v env
	
let rec runRobot (intructions : inst list) (pos:int) (env : (string * int) list ) : int option = 
	match intructions with
	  | (Step Left )      :: rest -> runRobot rest (pos - 1) env
	  | (Step Right)      :: rest -> runRobot rest (pos + 1) env
	  
	  | (Jump (Right, d)) :: rest -> (match fetch' d env with Some d' -> runRobot rest (pos + d') env | None -> None)
	  | (Jump (Left , d)) :: rest -> (match fetch' d env with Some d' -> runRobot rest (pos - d') env | None -> None)
	  
	  | (Set (s,d))       :: rest -> (match fetch' d env with Some d' -> runRobot rest (pos) ((s,d') :: env) | None -> None)
	  
	  | []                        -> Some pos
	
(* test on some examples *)
let t2 = runRobot [Set  ("x", I 3); Jump (Left, Var "x")]  0 []


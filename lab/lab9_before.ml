
(* Attendence *)

(* Part 1: reasoning with types *)

type exp = Add of exp * exp | Sub of exp * exp | I of int | Name of string


(* task: some names can be bound in expressoions, write a function that evaluates the expresion with the given type signature *)
(*
(string * int) list
(string * exp) list
(exp * exp) list
*)

let rec fetch (s: string) (env : (string * int) list) : int option =
  match env with
  | (x, i) :: xs -> if x = s then Some i
                    else fetch s xs
  | [] -> None

let rec eval (e:exp) (env : (string * int) list): int option =   (* ??? *)
  match e with 
  | Name x -> fetch x env
  | I i -> Some i
  | Add(x, y) -> (match (eval x env, eval y env) with
                | (Some x' , Some y') -> Some (x' + y')
                | _ -> None
                ) 
  | Sub(x, y) -> (match (eval x env, eval y env) with
                  | (Some x' , Some y') -> Some (x' - y')
                  | _ -> None
                  ) 
let el1 = [(Name "x", I 3), ("y", 4)]
  
let e1 = I 3
let e2 = Add (I 3, I 2)
let e3 = Add (Sub (I 3, I 5) , Sub (I 3, I 5) )
(* test on some examples *)



(* Part 2 : extending the robot example from lab 7 *)


(* 
rules in the form 
(p,S,m) → (p’,S’,m’)
p is a program
S is a stack
m is an environment


-----------------------------------------------
(Step Right :: rest, S, n) -> (rest, S, n + 1)


-----------------------------------------------
(Step Left :: rest, S, n) -> (rest, S, n - 1)


-----------------------------------------------
(Jump Right m :: rest, S, n) -> (rest, S, n + i)


                   x in m
-----------------------------------------------
(Jump Right x :: rest, S, n) -> (rest, S, n + fetch(x,m))



-----------------------------------------------
(Jump Left i :: rest, S, n) -> (rest, S, n - i)


                   x in m
-----------------------------------------------
(Jump Left x :: rest, S, n) -> (rest, S, n - fetch(x,m))


-----------------------------------------------------
(Set n i :: rest , S, n) -> (rest, n = i in S, n - i)


                                x in m
-------------------------------------------------------------
(Set n x :: rest , S, n) -> (rest, n = fetch(x,m) in S, n - i)


 *)

type dir = Left | Right

type dist = I of int | Var of string

type inst = Step of dir | Jump of dir * dist | Set of string * int
 
let rec fetch' (d: dist) (env : (string * int) list) : int option =
  match d with
  | I i -> Some i
  | Var v -> fetch v env

(* finish the function *)
let rec runRobot (intructions : inst list) (pos:int) (env : (string * int) list): int option =  (* ??? *)
  match intructions with 
  | (Step Left) :: rest -> runRobot rest (pos - 1) env
  | (Step Right) :: rest -> runRobot rest (pos + 1) env
  | (Jump (Right, d)) :: rest -> (match fetch' d env with
                                  | Some d' -> runRobot rest (pos + d') env
                                )
  | (Set (s, d)) :: rest 
(* test on some examples *)

(* if time: review strategies for begin...end *)

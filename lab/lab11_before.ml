(* Take attendence *)

(* Remind about the hints in lab 10 *)

(* robot from lab9, with slight imporvments based on the HW part 2 solutions *)

type value = VI of int

type dir = Left | Right

type exp = I of int | Var of string

type inst = Step of dir | Jump of dir * exp | Set of string * exp 



(* If we limit our interactions with the environment to these helper functions, we can safely change how we encode the environment without changing much other code *)
type env = (string * value) list
let insert (s:string)  (sv : value) (env: env) : env = (s,sv)::env

let rec fetch (name :string)  (env: (string * value) list) : value option = 
    match env with
      | (name' , v) :: rest -> if name = name' then Some v else fetch name rest
      | []                  -> None
      
let empEnv = []
  
let rec runRobot (intructions : inst list) (pos:int) (env : env) : int option = 
    let res (e:exp) = 
	   match e with
	     | Var s -> fetch s env
		 | I i   -> Some (VI i)
	in match intructions with
	  | (Step Left )      :: rest -> runRobot rest (pos - 1) env
	  | (Step Right)      :: rest -> runRobot rest (pos + 1) env

	  | (Jump (Right, d)) :: rest -> (match res d with Some (VI d') -> runRobot rest (pos + d') env | _ -> None)
	  | (Jump (Left , d)) :: rest -> (match res d with Some (VI d') -> runRobot rest (pos - d') env | _ -> None)
	  
	  | (Set (s,d))       :: rest -> (match res d with Some v -> runRobot rest pos (insert s v env) | _ -> None)
	  
	  | []                        -> Some (pos)

   

(* Add "procedures" to the value type, add "Run" to the instruction type

have procedures operate the robot, they will take in 1 named parameter, 
when procedures are defined they "remember" the current evironment 
when procedures are run they "forget" the bindings created locally

1st how should this be encoded in data types?
2nd make some tests that excersise the behavior
3rd modify the runRobot function
*)

(* Add a "Simulate" instruction: 
simulate takes a name of a procedure, a expression for it's parameter, a binding to inspect when the function is completed and a variable name that will be set to that value.
simulate will not move the robot.

1st how should this be encoded in data types?
2nd make some tests that excersise the behavior
3rd modify the runRobot function
*)



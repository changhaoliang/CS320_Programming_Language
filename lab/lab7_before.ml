(* Simple Robot simulater *)

(* 
this lab we will simulate a very simple robot that moves left and right based on a sequence of instrustions

the robot will operate based off this operaional semantics:

(step:: Right::xs     , n) -> (xs, n+1)
(Step: Right:: xs,     ,n) -> (xs, n-1)

(Jump:: Right :: i :: xs      ,n) -> (xs, n+ i)


Right: direction
n + 1 change  the direction
rest: rest instruction

(Step :: Right :: rest , n)      -> (rest, n + 1)
(Step :: Left :: rest , n)       -> (rest, n - 1)

(Jump :: Right :: i :: rest , n) -> (rest, n + i)
(Jump :: Left :: i :: rest , n)  -> (rest, n - i)

*)

(* write a type for instrustions *)
type instruct =  Step | Jump | Right | Left | Int of int;;
(* write a function that simulates the robot given a starting position *)
(* let runRobot *)
let incRobot (ls : instruct list) (n : int) : (instruct list * int) = 
  match ls with
  |Step :: Left :: xs -> (xs, n-1)
  |Step :: Right :: xs -> (xs, n+1)
  |Jump :: Right :: Int i :: xs -> (xs, n+ i)
  |Jump :: Left :: Int i :: xs -> (xs, n-i)
  |_ :: xs -> (xs, n)
  |[] -> ([], n)

let rec runRobot (ls :instruct list) (n : int): int =
  let (ls', n') = incRobot ls n in
    match ls' with
    | [] -> n'
    | _ -> runRobot ls' n'
(* take attendence *)

(* office hours *)

(* good luck on the midterm *)
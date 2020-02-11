(* Attendence *)


(* type inference  *)

type bar = A of int | B of bar list 

let foo x y z = 
  match (x, y) with
  | ((A i) :: zz, w :: ww) -> w + i 
  | _                      -> z (A 7)

(*x 是一个bar*)
let rec baz x = 
 foo [] [] baz
  
(*'a->'a->'b-> 'a  *)
let rec bam x y z = 
 bam (bam x x z) y z
 
;;
 
(* grammar review *)
 
(* consider the small subset of OCaml relating to bool list

Exp  ::= <Bool> :: <Exp> | <Exp> @ <Exp> | []
Bool ::= true | false

for example:
*)
true :: false :: [] @ false :: true :: []  @ [] @ false :: true :: [] ;;

(* is the grammar ambigous? *)

(* write a new grammar
* that accepts exactly the same strings the prevous grammar accepted
* gives :: higher precidence than @
* that is unambigous
*)


(* Q and A about more recent topics *)



(* 10-15 min of time to go over HW *)


(* Goog luck on the final *)


let rec my_seq3 () = 
  Cons (1, fun () ->
  Cons (2, fun () -> 
  Cons (3, zip (+) (my_seq3) (zip (+) (tail my_seq3) (tail (tail my_seq3))))))

  
let rec my_seq2 () = 
  Cons (1, fun () ->
  Cons (2, fun () ->
  Cons (4, zip (+) (my_seq2) ((tail my_seq2)))))
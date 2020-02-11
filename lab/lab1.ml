

(* types, values, and expressions *)

(* some values of type int *)
3;;
5;;
4;;

(* some (non value) expressions of type int *)
4 + 4;;
5 + 2 ;;
1 + 1 + 3 ;;
3 + 5;;
3 * 5 ;;
1 / 2 ;;

(* some values of type float *)
3.6 ;;
1.5;;
2.5 ;;
3.0 ;;
3. ;; (* note that the 0 in 3.0 is not needed in ocaml *)
1. ;;
0.2 ;; (* the leading 0 is required in ocaml *)

(* some (non value) expressions of type float *)
0.2 +. 0.2;; (* 0.2 + 0.2 is incorrect because + only operates over ints NOT floats *)
3.0 +. 2.5;; (* 3 + 2.5 is incorrect for the same reason *)
1.0 +. 2.0 ;;
1.5 +. 2.0;;
3.6 *. 2.5 ;; (* 3.6 * 2.5 is incorrect for the same reason *)
1.0 /. 2.0 ;; (* 1 / 2 is incorrect for the same reason *)


(* some values of type string *)
"hello" ;;
"hello world" ;;

(* some expressions of type string *)
"hello" ^ "world";;

(* some values of type char *)
'a' ;;
'c' ;;
(*
unlike c, chars are not comparable with integers 
'c' + 1
will not compile 
*)

(* the values of type bool *)
true;;
false;;
(*
note that bools are not comparable with integers (like in c), 
false == 0
will not compile 
*)


(* let expressions *)
(* assume we want to do some math with pi *)
3.14159265359 +. 3.14159265359 *. 3.14159265359 ;;
(* we can save some writing by using a let expression to locally decalair pi *)
let pi = 3.14159265359 in pi +. pi *. pi ;;

(* let expression examples *)
let x = "hi" in x ^ x ;;
let x = 3 in x + x ;; 
let y = 3 in y ;;
let x = 'a' in x ;;
let a = 10 - 4 in 7 + a + a ;;
let x = 3 + 4 in x + 2 ;;
let a = 3. +. 4. in 7.5 +. a ;;

let x = "hi" in 7 ;; (* variables don't need to be used (but will generate a warning) *)
let a = 'x' in 7 + 1;;
let x = 3 in 'a' ;;
let x = 7 in "hi" ;;
let x = 1 in (let x = 1.2 in x +. x) ;;


(* you can put a let anywhere an expression is expected *)
(let x = 7 in x) + (let x = 5 in x)  ;;
(let x = 1 in x+x) * (let x =3 in x *x) ;;

(* we can nest expressions in other expressions, what do they evaluate to? *)
let x = 3 in   let y = x + 1 in   x * y     ;; (* should be read as *)
let x = 3 in ( let y = x + 1 in ( x * y ) ) ;;

let x = 7 in  let y = x + 1 in  y + x   ;; (* should be read as *)
let x = 7 in (let y = x + 1 in (y + x) );;

let x = 3 in (let x = x + 1 in x ) * x ;; (* should be read as *)
let x = 3 in (let y = x + 1 in y ) * x ;;

let x = 7 in  x + let x = 5 in   x     ;; (* should be read as *)
let x = 7 in (x + let x = 5 in ( x ) ) ;; (* should be read as *)
let x = 7 in (x + let y = 5 in ( y ) ) ;;
 
let x = 1 in (x+x + (let x = x+1 in x *x) + x ) ;; (* should be read as *)
let x = 1 in (x+x + (let y = x+1 in y *y) + x ) ;;
 
(* more let examples *)
let x = 5 + 9 in let y = 0 + 8 in x + y ;;
let y = (let x = 3 in x + 2) in let z = y + 2 in y + z ;;
let x = 3 in let y = x + 1 in x * y ;;
let x = 3 in (let x = x + 1 in x ) * x ;;
let x = 1 in ( x + (let y = 2 in y + let z = 3 in x +y + z )) ;;
let x = 5 + 9 in (let y = 0 + 8 in x + y) + x ;;
let x = 1
  in let y = 2
     in x + y ;;
let x = 5 + 9 in (let y = 0 + 8 in x + y) + x ;;
let x = (let y = 2 in 2+2) in (let y = 1 in 1) ;;

(* the variable is only scoped to the end of the expression
let x = 3 in ( let y = x + 1 in ( x * y ) ) + y ;; 
so the last y is not in scope                 ^
and
let y = (let x = 3 in x + 2)
  in let z = y + 2
  in y + x;;
         ^   x is not in scope

*)

(* without "in" a variable will be in scope for the rest of the file *)
let x = 4 +5;;
x ;;
x  + x;;
let pi = 3.14 ;;

(* if then else expressions *)
if false then true else false ;;
if false then 10 else 14 ;;
if 9 > 3 then 10 else 5 ;;
if 9 > 3 then 10. else 5. ;;
if x > 4 then 8 else 10 ;;
let x = false in 
  if x == true then 4 + 2 else 4 - 2 ;;
let y = 5 in let z = 6 in 
  if true then y else z ;;
(*
the branches need to have the same types. the following expressions have type errors
if false then 10 else false ;;
if false then 10 else 10. ;;
if 9 > 3 then 10. else 5 ;;
if 9 > 3 then 10. else "dfdf" ;;
if 9 > 3 then 10. else 10 ;;
if x > 4 then 8 else 10. ;;

typing annotations can help you get better error messages
((if 9 > 3 then 10. else 10) : int) ;;
if false then (10. : int) else 10 ;;

*)


(* functions can be declared with let syntax *)
let f (x : int) : int = x + x
  in f 3 + f 4 ;;
let f (x : int) : int = x + x;; (* f has type  int -> int *)
f 5 ;;

let foo (var : string) : string = var ^ var ^ var 
  in  foo "hi" ^ foo "!";;
let foo (var : string) : string = var ^ var ^ var ;; (* foo has type string -> string *)
foo "123" ;;

let foo' (var : int)  = 0.0 ;; (* you don't need to use the vars in the body of the function *)
let add (x : int) (y: int) : int = x + y ;; (* functions can be defined with multiple arguments *)
add 1 2;;
let mult (x : int) (y : int) : int = x * y ;;
mult 3 5;;
mult 3 ;;
let g = mult 3 in g 4 ;;

(* the type annotations are usually optional *)
let f x = x + x;; (* f has type  int -> int, this only works because + is exact about its types *)
let f x = x +. x ;; (* f has type  float -> float *)

(* you need the rec keyword to use recursion *)
let rec pow (base : int) (exp : int) : int =
    if exp == 0
    then 1
    else base * pow base (exp - 1) ;;
pow 2 3;;

(* recursion can make functions the loop forever
let rec f x = f x in 7 + f 7 ;;
*)
(* what does this do ? *)
let rec f x = f x in 7 ;;


(* only the last lab made it this far *)

(* match expressions *)
 match 8 with
    1 -> true
  | _ -> false;;

(* use match in a function *)
let rec div3 (x : int) : bool =
   match x with
       1 -> false
     | 0 -> true
     | 2 -> false
     | y -> div3 (y - 3) ;;
	 
div3 9;;
div3 10;;

(* tuple expressions *)
(3 , "three" , 3.) ;;
match (1 , "hi") with (x, _) -> x ;;
match (1 , "hi") with (_, x) -> x ;;
match (1 , "hi") with
    (1 , x)  -> x
   |(_ , _)  -> "bye" ;;
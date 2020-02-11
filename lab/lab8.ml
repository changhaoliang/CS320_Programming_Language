(* Take attendance *)

(* review Project part 1, will be posted separately *)


(* midterm review  *)

(* from appendix  *)
let rec length (l: 'a list):int =
 match l with
 | []-> 0
 | hd::tl -> 1 + (length tl)
let rec map (f: 'a -> 'b) (l: 'a list): ('b list)=
 match l with
 | [] -> []
 | hd::tl -> (f hd):: (map f tl)
let rec filter (p: 'a -> bool) (l: 'a list): 'a list =
 match l with
 | []-> []
 | hd::tl -> if (p hd) then hd::(filter p tl) else
 (filter p tl)
let rec fold_left (f:'a->'b->'a) (acc:'a) (l:'b list): 'a =
 match l with
 | [] -> acc
 | x::xs -> fold_left f (f acc x) xs
let rec append (xs:'a list) (ys:'a list): 'a list =
 match xs with
 | [] -> ys
 | z::zs -> z :: (append zs ys)
;;


(* 16 *)

let l = [[0;8]; [-11;5]; [7;2]] in
let foo = fold_left (fun a x -> a + (2 * x) - 1) 0 in
foo (map foo l)
;;
(* with explicit types *)
let l : int list list = [[0;8]; [-11;5]; [7;2]] in
let foo : int list -> int = fold_left (fun a x -> a + (2 * x) - 1) 0 in
(foo (map foo l : int list) : int)

(* by process of elimination the answer is B *)

(* 17 *)

(* explanation given in lab had technical problems, ask on piazza if you are still interested *)

(* 22 *)

let rec fold' f acc p l=
  match l with
   | [] -> acc
   | x::xs ->
        if (p x) then fold' f (f acc x) p xs
        else fold' f acc p xs

(* for fold'
from the match, for some 'x
l : 'x list

from first case, if acc : 'acc
fold' f acc p l : 'acc

in the last case, x : 'x , xs : 'x list
from (p x),
p : 'x -> bool

from (f acc x),
f : 'acc -> 'x -> 'acc

leaving
fold' : ('acc -> 'x -> 'acc )-> 'acc -> ('x -> bool) -> 'x list -> 'acc
or
fold' : ('a -> 'b -> 'a )-> 'a -> ('b -> bool) -> 'b list -> 'a
*)

let bar m c= fold'
             append []
             (fun s-> (length s) > 1) m
(* for bar
c is never used so it has any type c : 'c

append : 'x list -> 'x list -> x list
so when in the first position of fold' : ('a -> 'b -> 'a )-> 'a -> ('b -> bool) -> 'b list -> 'a
'a = 'x list
'b = 'x list

form this, m : 'x list list
so
bar : 'x list list -> 'c -> 'x list
or
bar : 'a list list -> 'b -> 'a list
*)
			 
			 
let foo m c= fold'
             append []
             (fun _ -> true )
             (filter (fun z-> (length z)> 1) m)
(* 
the same logic above applies to foo *)

(* 23 *)

(* the answer has at least the following constructors *)
type ty = B of int | P of int * int | Empty 

(* 
the data type must have every constructor it is pattern matched with
and each constructor must be "of" the correct type
*)

let rec myfun x y z =
 match (x, y) with
 | (0, _) -> Empty
 | (n, B i) -> B (i * z * n)
 | (n, P (a, b)) ->
(match (myfun a y z, myfun b y z) with
 | (Empty, Empty) -> Empty
 | (B i, B j) -> B (i + j)
 | _ -> Empty)
 | (_, Empty) -> Empty
 
(* part 2: the answer has at least the following constructors *)
type ty = B of float | P of ty * ty | Empty 

(* 
the data type must have every constructor it is pattern matched with
and each constructor must be "of" the correct type
*)
let rec myfun x y z =
 match (x, y) with
 | ([], _) -> Empty
 | (s::xs, B i) -> B (2.0 +. i)
 | (s::xs, P (a, b)) ->
(match (myfun x a z, myfun x b z) with
 | (Empty, Empty) -> Empty
 | (B i, B j) -> B (i *. j)
 | _ -> Empty)
 | (_, Empty) -> Empty
 
 
(* 24
Some examples from the Language
id+id+id
id+-id*id+id
id
-------------------------------------------id
Some examples NOT in the Language
id+(id*id)             no parens
id-id                  so subtraction
idid                   ids must be separated


Explain why this grammar is ambiguous:
id+id+id is one example sentence, parse trees were required for total credit

Rewrite the grammar so that – has precedence over + and * . And * has precedence over +
A ::= <A> + <A> | <T>
T ::= <T> * <T> | <M>
M ::= -<M> | id

Rewrite the grammar so that it is unambiguous and + is right-associative and * is left-associative
A ::= <T> + <A> | <T>
T ::= <T> * <M> | <M>
M ::= -<M> | id
this would also be a correct answer for the above prob
*)



 
  
type 'a str = Cons of 'a * ('a stream)
   and  'a stream = unit -> 'a str


let head (s: 'a stream) = let Cons(a, _) = s() in a
let tail (s: 'a stream) = let Cons(_, tl)= s() in tl

let rec take (n:int) (s: 'a stream) : 'a list = 
  if n>0 then 
  (head s) :: (take (n-1) (tail s))
  else []

let rec map (f: 'a -> 'b) (s:'a stream) : 'b stream =
  fun () -> Cons (f (head s), map f (tail s))
                      
let rec zip f s1 s2 () = Cons (f (head s1) (head s2), zip f (tail s1) (tail s2))

let rec filter (s: 'a stream) (p: 'a -> bool) : 'a stream = 
   if p (head s)  then fun () -> Cons (head s, filter (tail s) p)
   else 
     (filter (tail s) p)

let rec sieve (s: int stream) : int stream =
     fun () -> Cons(head s, 
        sieve (filter (tail s) (fun x -> x mod (head s) <> 0)))

let rec nums_from (i: int) : int stream = fun () -> Cons (i, nums_from (i+1))

let rec nats : int stream = nums_from 0

let primes : int stream = sieve (nums_from 2)



let even x= (x mod 2) = 0
let odd  x= (x mod 2) = 1

let rec fibs () = Cons (0,fun () -> Cons(1, zip (+) fibs (tail fibs)))
let rec ones = fun () -> Cons (1, ones)
let rec zeros = fun () -> Cons (0, zeros)
                        
                               (*************************************)
                               (*************************************)
                               (******  ASSIGNMENT STARTS HERE ******)
                               (*************************************)
                               (*************************************)

(*  define evens: int stream
 *  as the stream of all even natural 
 *  numbers
 *)
let (evens: int stream) = map (fun x -> 2* x) nats
 (* write here your code *)

(*  define odds: int stream
 *  as the stream of all odd natural 
 *  numbers
 *)

let (odds: int stream) = map (fun x -> 2*x + 1) nats 
 (* write here your code *)

(*   
  define squares : int stream
  as the stream of all perfect squares
  of natural numbers. 
*)
let (squares : int stream) = map (fun x -> x * x) nats 
 (* write here your code *)

(*   
    define evenFibs: int stream
    the stream of even Fibonacci numbers
    in increasing order.
 *)




let (evenFibs: int stream) = filter fibs even
 (* write here your code *)

(* 
  define mulThrees: int stream 
  the stream of multiples of three 
  in increasing order.
 *)
let (mulThrees: int stream) = map (fun x -> x * 3) nats
 (* write here your code *)


(* 
  Define a function split_on_p: 'a stream -> ('a -> bool) -> ('a stream) * ('a stream) 
  that takes as inputs a stream of elements of type 'a and a
  predicate p:'a -> bool over 'a and returns a pair of  streams  where the
  first stream contains the elements of the input stream that satisfy
  p, while the second stream contains  the elements of the
  input stream that do not satisfy p.
 *)

 let rec filter_false (s: 'a stream) (p: 'a -> bool) : 'a stream = 
  if p (head s) == false  
  then fun () -> Cons (head s, filter_false (tail s) p)
  else (filter_false (tail s) p)

 let rec split_on_p (a:'a stream) (p: 'a -> bool) : ('a stream) * ('a stream) =  
  (* write here your code *)
   (filter a p, filter_false a p)


(* 
 *  define a function stream_zip: 'a stream -> 'b stream -> ('a * 'b) stream that
 *  takes two streams of elements and returns a stream of the paired elements.
 *  For instance given the streams of natural numbers greater than 0,  
 *  and the streams of primes, stream_zip should return a stream of pairs of integers 
 *  where each pairs is of the form (i, pi) where pi is the i-th prime number, for i>0.
*)
let rec stream_zip (s1: 'a stream) (s2: 'b stream): ('a * 'b) stream = 
 (* write here your code *)
 fun () -> Cons ((head s1, head s2), (stream_zip (tail s1) (tail s2)))



(* 
  define a function
  stream_unzip: (’a * ’b) stream -> (’a stream * ’b stream)
  that takes as inputs a stream of pairs of elements of type ’a * ’b, 
  and produces a pair of streams, the first containing only elements 
  of type ’a and the second containing only elements of type ’b.
 *)

 let rec helper1 (abstream : ('a * 'b) stream) : 'a stream = 
  match abstream() with
    Cons ((x, y), tl) -> fun () -> Cons(x, helper1 tl)

let rec helper2 (abstream : ('a * 'b) stream) : 'b stream = 
  match abstream() with
    Cons ((x, y), tl) -> fun () -> Cons(y, helper2 tl)

let rec stream_unzip (abstream : ('a * 'b) stream) : ('a stream * 'b stream) = 
 (* write here your code *)
  (helper1 abstream, helper2 abstream)

(* 
 *  define a function incremental_map : ('a -> 'a ) -> a' -> 'a stream
 *  that given a function f, and an element a of type 'a 
 *  produces the stream of elements a, (f a), (f (f a)), (f (f (f a))), (f (f (f (f a)))).... 
 *)
let rec incremental_map (f: 'a -> 'a) (a: 'a): 'a stream = 
 (* write here your code *)
 fun () -> Cons(a, incremental_map f (f a))

(*
 *  Use incremental_map to define a stream of floats called exp_seq in which the i-th element is 
 *  the (i-1)-th element squared, starting from 2, i.e. 2, 4, 16, 256, 65536...
 *  This stream can be seen as the sequence of values of the mathematical sequence
 *  n-> exp(2, exp(2, n)), for n=0,1,2...
 *)
let (exp_seq: float stream) = incremental_map (fun x -> x *. x) 2.0
 (* write here your code *)






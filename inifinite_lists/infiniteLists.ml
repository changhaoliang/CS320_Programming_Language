(*type 'a stream = Cons of 'a * ('a stream)

let head (s :'a stream) : 'a =
  match s with
    Cons (hd,_) -> hd

let tail (s :'a stream) : 'a stream =
  match s with
    Cons (_,tl) -> tl

let rec ones = Cons (1, ones)

let rec nats (i: int) : int stream = Cons (i, nats (i+1))

let n = nats 0*)



      

      
type 'a str = Cons of 'a * ('a stream)
and  'a stream = unit -> 'a str
   
let head (s :'a stream) : 'a =
  match s() with
    Cons (hd,tl) -> hd
                      
let tail (s :'a stream) : 'a stream =
  match s() with
    Cons (hd,tl) -> tl

let rec nats (i: int) : int stream = fun () -> Cons (i, nats (i+1))

let rec take (n:int) (s: 'a stream) : 'a list = 
  if n>0 then 
  (head s) :: (take (n-1) (tail s))
  else []

let rec map (f: 'a -> 'b) (s:'a stream) : 'b stream =
  fun () -> Cons (f (head s), map f (tail s))
                      
let rec ones = fun () -> Cons (1, ones)
let even x= (x mod 2) = 0
let odd  x= (x mod 2) = 1
let rec ones () = Cons (1, ones)

let rec zeros () = Cons (0, ones)

let twos = map (fun x -> x+1) ones

let rec zip (f:'a -> 'b ->'c) (s1: 'a stream) (s2: 'b stream) : 'c stream =
  fun () -> Cons (f (head s1) (head s2), zip f (tail s1) (tail s2))

let threes = zip (+) ones twos

let rec fibs () = Cons (0,fun () -> Cons(1, zip (+) fibs (tail fibs)))

let (evens: int stream) = map (fun x -> 2* x) (nats 0)

let rec filter (s: 'a stream) (p: 'a -> bool) : 'a stream = 
if p (head s) 
then fun () -> Cons (head s, filter (tail s) p)
else (filter (tail s) p)

let rec filter_false (s: 'a stream) (p: 'a -> bool) : 'a stream = 
  if p (head s) == false  
  then fun () -> Cons (head s, filter (tail s) p)
  else (filter (tail s) p)

let rec sieve (s: int stream) : int stream =
     fun () -> Cons(head s, 
        sieve (filter (tail s) (fun x -> x mod (head s) <> 0)))
(*
let let primes : int stream = sieve (nats 2)
*)
let rec split_on_p (a:'a stream) (p: 'a -> bool) : ('a stream) * ('a stream) =  
  (* write here your code *)
   (filter a p, filter_false a p)

let rec helper1 (abstream : ('a * 'b) stream) : 'a stream = 
  match abstream() with
    Cons ((x, y), tl) -> fun () -> Cons(x, helper1 tl)

let rec helper2 (abstream : ('a * 'b) stream) : 'b stream = 
  match abstream() with
    Cons ((x, y), tl) -> fun () -> Cons(y, helper2 tl)

let rec stream_unzip (abstream : ('a * 'b) stream) : ('a stream * 'b stream) = 
(* write here your code *)
  (helper1 abstream, helper2 abstream)
    


(* a simplified version of the solution to part 2, fill in the blanks to get functions working *)

type stackVal = 
    I of int 
  | N of string
  | U
  | Fun  (* of  ??? *)
  | IoFun (* of  ??? *)
  | E
  (* ... *)
and command  PushI of stackVal 
  | PushN of stackVal 
  | Neg
  | Begin | End
  | Call
  | FunDef (* of  ??? *)
  | IoFunDef (* of  ??? *)
  | Return
  | Bind
  (* ... *)


(* some preliminaries from the solution to part 2 *)
and env = (string * stackVal) list list

let insert (s:string)  (sv : stackVal) (env: env) : env = 
    match env with
      | frame :: frames  -> ((s, sv) :: frame) :: frames
      | []               -> [[(s, sv)]]

let rec fetch (name :string)  (env: env) : stackVal option = 
    match env with
      | ((name' , v) :: frame) :: frames  -> if name = name' then Some v else fetch name (frame :: frames)
      | []                     :: frames  -> fetch name frames
      | []                                -> None
      
let empEnv = [[]]

let pushEnv  (env: env) : env  = [] :: env

let popEnv (env: env) : env  = 
  match env with
    | [] -> empEnv
    | _ :: rest ->  rest

	
let insertStack  (sv: stackVal) (stack: stackVal list list) : stackVal list list  = 
  match stack with
    | [] -> [[sv]]
    | h :: r  -> (sv :: h) :: r


let rec run (commands : command list) (stack: stackVal list list) (env: env) : (* TODO *) (stackVal list) = 
  let res (sv : stackVal) : stackVal = 
    match sv with 
      | N n -> (match fetch n env with  
                  | Some n' -> n' 
                  | None -> N n)
      | sv -> sv
  in let bad rest = (run rest (insertStack E stack) env)
  in match (commands , stack)  with
  
  | ([]                 , s :: _                ) -> s
  
  | (PushI (I i) :: rest, _                     ) -> run rest (insertStack (I i) stack) env
  
  | (PushN (N n) :: rest, _                     ) -> run rest (insertStack (N n) stack) env
  
  | (Neg         :: rest, (x ::s')     :: frames) -> (match (res x) with 
                                                        | (I i) -> run rest ((I (-i) :: s' ) :: frames) env 
                                                        | _ -> bad rest)
  
  | (Bind        :: rest, ( N n::x::s'):: frames) -> (match res x with
                                                        | E   -> bad rest 
                                                        | N _ -> bad rest (* unbound var *)
                                                        | v -> run rest ((U :: s'):: frames) (insert n v env) )

  | (Begin       :: rest, frames                ) -> run rest ([] :: frames) (pushEnv env)
  | (End         :: rest, ((top :: _) :: frames)) -> run rest (insertStack top frames) (popEnv env)

  | (FunDef (* TODO *)   :: rest, _ )  -> let closure = Fun (* TODO *) 
                                          in bad rest  (* TODO *)
  
  | (IoFunDef (* TODO *)  :: rest, _)  -> let closure = IoFun (* TODO *) 
                                          in  bad rest  (* TODO *)


  | (Call :: rest, (x::n::s') :: frames) -> 
      (match (res n, res x) with 
        | (_ ,E)   ->  bad rest (* errors are not given as inputs to functions *)
        | (_, N _) ->  bad rest (* unbound var *)
        | (Fun , v) -> 
        (*
          let (env'', (top :: _)) = run ? ? ?
          
          in let top' = match top with N n -> let (Some v) = fetch n env'' in v | v -> v
          in run ? ? ?
          *)
           bad rest  (* TODO *)
        | (IoFun , v) -> 
        (*
           let (N calledWithName) = x
           
           in let (env'', _) = run ? ? ?
           in let (Some newValue) = fetch argname env''
           in run ? ? ?
          *)
           bad rest  (* TODO *)
        | _ -> bad rest )
                                          
  | (Return      :: _   , s :: _                ) -> s
  
  | (_ :: rest, _ ) -> bad rest
    

(* Always test the code *)

  (*
let e11 = run [IoFunDef ("f", "x",
    [PushI (I 0); PushN (N "x"); Bind]);
   PushI (I 1); PushN (N "y"); Bind; 
   PushN (N "f"); PushN (N "y"); Call;
   PushN (N "y"); Neg] [[]] empEnv

let e12 = run [FunDef ("f", "x",
  [PushI (I 0); PushN (N "y"); Bind; 
 PushN (N "y");
  Return]);
 PushN (N "f"); PushI (I 5); Call]  [[]] empEnv
 *)
 
 
(* go over parsing in the solution code *)

(* go over the alternate solution *)


(* If time allows review some standard functions on list *)

type 'a myList = MyNil | MyCons of 'a * ('a myList)

let e = MyCons (0, MyCons (1, MyCons (2, MyNil)))


let take (n : int) (ls : 'a myList) : 'b list = (* TODO *) []

let map (f : 'a -> 'b) (ls : 'a myList) : 'b myList = (* TODO *) MyNil

let zip (f: 'a -> 'b -> 'c) (s1: 'a myList) (s2: 'b myList) : 'c myList = (* TODO *) MyNil

let filter (s: 'a myList) (p: 'a -> bool) : 'a myList = (* TODO *) MyNil

(* If time allows start to talk about infinite lists *)

(* teacher review *)
 
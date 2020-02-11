(* things that goes on a stack *)


type stackVal = 
  I of int 
| S of string 
| N of string
| B of bool 
| U
| E

(* well formed instructions *)
type command = PushI of stackVal 
           | PushS of stackVal 
           | PushN of stackVal 
           | PushB of stackVal
           | Push of stackVal
           | Add | Sub | Mul | Div | Rem | Neg
           | Concat
           | And | Or | Not
           | Equal | LessThan
           | If
           | Pop
           | Swap
           | Begin | End (* This solution uses seperate commands for Begin ... End *)
           | Bind
           | Quit
           | Fun of stackVal * stackVal
           | FunEnd
           | Return
           | InOutFun of stackVal * stackVal
           | Call
(* writing *)
let to_string (s : stackVal) : string = 
  match s with
  | I i -> string_of_int i 
  | S s -> s
  | N n -> n
  | B b -> "<" ^ string_of_bool b ^ ">"
  | U   -> "<unit>"
  | E   -> "<error>"

type env = (string * stackVal) list list

type closure = 
  env * command list * string
type func_env = (string * closure * int) list list

type func_map = (string * func_env) list

let rec print_env (ls : (string*stackVal) list list)(x : string) : string = 
  match ls with
  | ((s, v) :: xs) :: rest -> print_env (xs :: rest) (x ^" " ^ s^ ":" ^ (to_string v)^"; ")
  | [] :: rest ->  print_env (rest) (x ^ "\n" )
  | [] -> x 
let test = [[("a", (I 10)); ("b", (B true))];[("v", (I 5))]]

let rec print_func_env (ls :(string * closure * int) list list)(x : string) : string = 
  match ls with
  | ((s, c, t) :: xs) :: rest -> print_func_env (xs :: rest) (x ^": " ^s ^"; ")
  | [] :: rest -> print_func_env rest (x ^ "\n")
  | [] -> x
let rec print_stack(stack :stackVal list list)(s : string) : string = 
  match stack with
  | (x::xs)::frames -> print_stack (xs :: frames) (s ^ (to_string(x))^"; " )
  | [] :: frames -> print_stack (frames ) (s ^ "\n")
  | [] -> s

let insert (s:string)  (sv : stackVal) (env: env) : env = 
  match env with
    | frame :: frames  -> ((s, sv) :: frame) :: frames
    | []               -> [[(s, sv)]]

let rec fetch (name :string)  (env: env) : stackVal option = 
  match env with
    | ((name' , v) :: frame) :: frames  -> if name = name' then Some v else fetch name (frame :: frames)
    | []                     :: frames  -> fetch name frames
    | []                                -> None
    
let rec fetch_func (name : string) (func_env : func_env) : (int * closure) option = 
match func_env with
|  ((name', closure, t) :: frame) :: frames -> if name = name' then Some (t, closure) else fetch_func name (frame :: frames)
|  []                             :: frames -> fetch_func name frames
|  []                                       -> None

let empEnv = [[]]
let pushEnv  (env: env) : env  = [] :: env

let popEnv (env: env) : env  = 
  match env with
  | [] -> empEnv
  | _ :: rest ->  rest

let push_func_env (func_env : func_env) : func_env = [] :: func_env
let pop_func_env (func_env : func_env) : func_env = 
  match func_env with
  | [] -> empEnv
  | _ :: rest -> rest

let rec restore_env (env : env) (num : int) : env =
  if (List.length env == num) then env
  else restore_env (popEnv env) num
  
let rec restore_func_env (func_env : func_env) (num : int) : func_env =
  if (List.length func_env == num) then func_env
  else restore_func_env (pop_func_env func_env) num
  
let insertStack  (sv: stackVal) (stack: stackVal list list) : stackVal list list  = 
match stack with
  | [] -> [[sv]]
  | h :: r  -> (sv :: h) :: r

let rec save_func(commands: command list)(new_command : command list)(func_num : int) : (command list*command list) =
match commands with
| xs :: ls -> (match xs with
              | FunEnd -> (if func_num == 0 then (new_command @ [FunEnd], ls)
                           else save_func ls (new_command @ [FunEnd]) (func_num -1) )
              | Fun (n1, n2) -> save_func ls (new_command @ [Fun (n1, n2)]) (func_num + 1)
              | _ -> save_func ls (new_command @ [xs]) func_num)

| _ -> save_func commands new_command func_num

let bind_func (n1 : string) (c : closure) (t : int) (func_env : func_env) : func_env = 
match func_env with
| frame :: frames -> ((n1, c, t) :: frame) :: frames
| []              -> [[n1, c, t]]

let push_map (n1 : string) (func_env: func_env)(func_map : func_map) : func_map = (n1, func_env) :: func_map

let rec fetch_map (n1 : string) (func_map : func_map) : func_env option = 
  match func_map with
  | (n, env) :: rest -> (if (n = n1) then Some env
                        else fetch_map n1 rest)
  | [] -> None

let rec print_res (s : stackVal list)(res: string) : string = 
match s with
| xs :: ls -> print_res ls (res ^ (to_string xs)) 
| [] -> res


let rec next_func (func_env : func_env) :  string =
  match func_env with
  | ((s, c, t) :: xs) :: rest -> s
  | [] :: rest -> next_func rest  
  | [] -> ""

let rec run (commands : command list) (stack: stackVal list list) (env: env) (func_env : func_env) (name1 : string)(var : string)(name2 : string Stack.t) 
(num_stack : int Stack.t) (flag : int) (num_stack1 : int Stack.t) (func_map:func_map): (stackVal list) = 
(* if stackVal is a variable what  does it resolve to in the current environment *)
let res (sv : stackVal) : stackVal = 
  match sv with 
    | N n -> (match fetch n env with  
                | Some n' -> n' 
                | None -> N n)
    | sv -> sv
in let bad rest : (stackVal list) = (run rest (insertStack E stack) env func_env name1 var name2 num_stack flag num_stack1 func_map) (* every command fails in the same way *) in
  match (commands , stack)  with
    | (PushI (I i) :: rest, _                      ) -> run rest (insertStack (I i) stack) env func_env name1 var name2 num_stack flag num_stack1 func_map
      
    | (PushS (S s) :: rest, _                      ) -> run rest (insertStack (S s)  stack) env func_env name1 var name2 num_stack flag num_stack1 func_map
    
    | (PushN (N n) :: rest, _                      ) -> run rest (insertStack (N n) stack) env func_env  name1 var name2 num_stack flag num_stack1 func_map
    
    | (PushB (B b) :: rest, _                      ) -> run rest (insertStack (B b) stack) env func_env name1 var name2 num_stack flag num_stack1 func_map
    
    | (Push U      :: rest, _                      ) -> run rest (insertStack U stack) env func_env name1 var name2 num_stack flag num_stack1 func_map
    | (Push E      :: rest, _                      ) -> run rest (insertStack E stack) env func_env name1 var name2 num_stack flag num_stack1 func_map

    | (Fun (N n1, N n2)::rest, _                  ) -> let (func_command, rest) = save_func rest [] 0 in
                                                        let c = (env, func_command, n2) in 
                                                        let func_env = bind_func n1 c 0 func_env in
                                                        let func_map = push_map n1 func_env func_map in 
                                                        run rest (insertStack U stack) env func_env name1 var name2 num_stack flag num_stack1 func_map

    | (FunEnd :: rest, (x :: s')::s''::frames          ) ->  
            let flag = flag - 1 in
            (match fetch_func name1 func_env with
            | Some (t, c) -> (
                match t with
                  | 1 -> (match fetch var env with
                        | Some v' -> 
                            let env = popEnv env in  
                            (if Stack.length name2 > 1 then (
                              let _ = Stack.pop name2 in
                              let env =  insert (Stack.top name2) v' (restore_env env (Stack.pop num_stack)) in 
                              let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                              run rest (s''::frames) env func_env (next_func func_env) var name2 num_stack flag  num_stack1 func_map
                            )
                            else (
                              let env =  insert (Stack.top name2) v' (restore_env env (Stack.pop num_stack)) in 
                              let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                              run rest (s''::frames) env func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map
                            )
                            
                            )
                        | _ -> let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                              run rest (s''::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map)
                  | 0 -> let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                      run rest (s''::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map)   
            | None -> bad rest)

            
    | (Return::FunEnd::rest, (x :: s')::s''::frames   ) -> (
            let flag = flag - 1 in 
            match fetch_func name1 func_env with
            | Some (t, c) -> (
              match t with
                | 1 -> (match fetch var env with
                      | Some v' ->  
                      (if Stack.length name2 > 1 then (
                        let _ = Stack.pop name2 in
                        let env = (insert (Stack.top name2) v' (restore_env env (Stack.pop num_stack))) in
                        let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                        run rest (((res x) :: s'')::frames) env func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map
                      )
                      else (
                        let env = (insert (Stack.top name2) v' (restore_env env (Stack.pop num_stack))) in
                        let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                        run rest (((res x) :: s'')::frames) env func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map
                      )) 
                          
                      | _ ->   let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                              run rest (((res x) :: s'')::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map)
                | 0 -> 
                match x with
                | N str -> (
                  match fetch str env with
                  | Some v -> 
                        let func_env = (restore_func_env  func_env (Stack.pop num_stack1)) in 
                        run rest (((res x) :: s'')::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map
                  | _ -> (
                        match fetch_func str func_env with
                        | Some (t, c) -> 
                              let func_env = (bind_func str c t (restore_func_env func_env (Stack.pop num_stack1))) in
                              let func_map = push_map str func_env func_map in
                              run rest ((x :: s'')::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map
                        | _ ->   let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in 
                                 run rest (((res x) :: s')::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map
                  )
                )
                | _ ->  let func_env = (restore_func_env func_env (Stack.pop num_stack1)) in
                        run rest (((res x) :: s'')::frames) (restore_env env (Stack.pop num_stack)) func_env (next_func func_env) var name2 num_stack flag num_stack1 func_map

                )  
            | None -> bad rest)
              
    | (InOutFun (N n1, N n2):: rest, _            ) ->  let (func_command, rest) = save_func rest [] 0 in
                                                        let c = (env, func_command, n2) in 
                                                        let func_env = bind_func n1 c 1 func_env in
                                                        let func_map = push_map n1 func_env func_map in 
                                                        run rest (insertStack U stack) env func_env name1 var name2 num_stack flag num_stack1 func_map 
    | (Call        :: rest, (x ::y ::s') :: frames) -> 
    let flag = flag + 1 in

    let len = List.length env in
    let len1 = List.length func_env in

    let _ = Stack.push len num_stack in 
    let _ = Stack.push len1 num_stack1 in
      (match y with
      | N name1 -> (match fetch_func name1 func_env with
                   | Some (t1, (e1, func_code, n2)) -> ( 
                     
                          match fetch_map name1 func_map with
                          | Some fe1 -> (
                            
                                  match t1 with
                                    (*Fun*)
                                    | 0 -> (match x with
                                            | N name22 -> (
                                                let _ = (Stack.push name22 name2) in 
                                                (* formal name has a stackVal *)
                                                  if fetch name22 env != None then (
                                                          match fetch name22 env with
                                                          | Some par -> 
                                                              run (func_code @ rest) ([]::s'::frames) (insert n2 par ( (e1 @ env))) (fe1 @ func_env) name1 var name2 num_stack flag num_stack1 func_map)

                                                  (*formal name is a function*)
                                                  else if fetch_func name22 func_env != None then (
                                                    match fetch_func name22 func_env with
                                                    | Some (t2, (e2, func_code2, n22)) -> 
                                                        match fetch_map name22 func_map with
                                                        | Some fe2 ->
                                                            let func_env = (bind_func n2 (e2, func_code2, n22) t2 (fe2 @ func_env)) in
                                                            let func_map = push_map n2 (func_env) func_map in
                                                            run (func_code @ rest) ([]::s'::frames) (e1 @ env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                        | None -> bad rest)

                                                  else bad rest )
                                                  
                                            | E -> bad rest 
                                            | _ ->  
                                                    run (func_code @ rest) ([]::s'::frames) (insert n2 x ( (e1 @ env))) (fe1 @ func_env) name1 var name2 num_stack flag num_stack1 func_map)
                                            
                                    (*InOutFun*)
                                    | 1 -> (
                                      match x with
                                            | N name22 -> (
                                              let _ = (Stack.push name22 name2) in 
                                              (* formal name has a stackVal *)
                                                if fetch name22 env != None then (
                                                    
                                                        match fetch name22 env with
                                                        | Some par -> 
                                                            run (func_code @ rest) ([]::s'::frames) (insert n2 par (pushEnv (e1 @ env))) (fe1 @ func_env) name1 n2 name2 num_stack flag num_stack1 func_map 
                                                )
                                                (* formal name is a function *)
                                                else if fetch_func name22 func_env != None then 
                                                    match fetch_func name22 func_env with
                                                    | Some (t2, (e2, func_code2, n22)) -> 
                                                        match fetch_map name22 func_map with
                                                        | Some fe2 -> 
                                                            let func_env = (bind_func n2 (e2, func_code2, n22) t2 (fe2 @ func_env) ) in
                                                            let func_map = push_map n2 func_env func_map in
                                                            run (func_code @ rest) ([]::s'::frames) (pushEnv (e1 @ env)) func_env name1 n2 name2 num_stack flag num_stack1 func_map
                                                        | None -> bad rest
                                                    else bad rest )
                                            | E -> bad rest 
                                            | _ -> let _ = Stack.push " " name2 in 
                                                    run (func_code @ rest) ([]::s'::frames) (insert n2 x (pushEnv (e1 @ env))) (fe1 @ func_env) name1 n2 name2 num_stack flag num_stack1 func_map
                                            )
                          )
                          | None -> let _ = print_string name1 in bad rest
                        )
                    | None -> bad rest
                )
      | _  -> bad rest)

    | (Add         :: rest, (x ::y ::s') :: frames) -> ( 
                                                          if (flag > 0 && (x == (N var) || y == (N var))) then 
                                                              (match (res x, res y) with 
                                                                | (I i, I j) -> run rest ((I (i+j) :: s' ) :: frames) (insert var (I (i+j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                                | _ -> bad rest)
                                                          else (match (res x, res y) with 
                                                                  | (I i, I j) -> run rest ((I (i+j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                                  | _ -> bad rest))

    | (Sub         :: rest, (x ::y ::s') :: frames) -> (
                                                          if (flag > 0 && (x == (N var) || y == (N var))) then 
                                                            (match (res x, res y) with 
                                                              | (I i, I j) -> run rest ((I (i-j) :: s' ) :: frames) (insert var (I (i - j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                              | _ -> bad rest)
                                                          else (match (res x, res y) with 
                                                              | (I i, I j) -> run rest ((I (i-j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                              | _ -> bad rest)
                                                          )
      
    
    
    | (Mul         :: rest, (x ::y ::s') :: frames) -> (  
                                                          if (flag > 0 && (x == (N var) || y == (N var))) then  
                                                          (
                                                          match (res x, res y) with 
                                                          | (I i, I j) -> run rest ((I (i*j) :: s' ) :: frames) (insert var (I(i * j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)
                                                          else (
                                                            match (res x, res y) with 
                                                            | (I i, I j) -> run rest ((I (i*j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                            | _ -> bad rest)
                                                          )
    
    | (Div         :: rest, (x ::y ::s') :: frames) -> ( 
                                                          if (flag > 0 && (x == (N var) || y == (N var))) then  
                                                          (
                                                            match (res x, res y) with 
                                                            | (I i, I 0) -> bad rest
                                                            | (I i, I j) -> run rest ((I (i/j) :: s' ) :: frames) (insert var (I (i / j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                            | _ -> bad rest)
                                                          
                                                          else (
                                                            match (res x, res y) with 
                                                            | (I i, I 0) -> bad rest
                                                            | (I i, I j) -> run rest ((I (i/j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                            | _ -> bad rest) 
                                                          )
      
    

    | (Rem         :: rest, (x ::y ::s') :: frames) -> (
                                                          if (flag > 0 && (x == (N var) || y == (N var))) then  
                                                          (
                                                            match (res x, res y) with 
                                                            | (I i, I 0) -> bad rest
                                                            | (I i, I j) -> run rest ((I (i mod j) :: s' ) :: frames) (insert var (I (i mod j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                            | _ -> bad rest)
                                                          
                                                          else (
                                                            match (res x, res y) with 
                                                            | (I i, I 0) -> bad rest
                                                            | (I i, I j) -> run rest ((I (i mod j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                            | _ -> bad rest))
    
    
    | (Neg         :: rest, (x ::s')     :: frames) -> ( if (flag > 0 && x == (N var)) then
                                                        (
                                                          match (res x) with 
                                                          | (I i) -> run rest ((I (-i) :: s' ) :: frames) (insert var (I (-i)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest
                                                        )
                                                        else (
                                                          match (res x) with 
                                                          | (I i) -> run rest ((I (-i) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest
                                                        )
    )

    | (Concat      :: rest, (x ::y ::s') :: frames) -> (if (flag > 0 && (x == (N var) || y == (N var))) then 
                                                        (match (res x, res y) with 
                                                          | (S i, S j) -> run rest ((S (i ^ j) :: s' ) :: frames) (insert var (S (i ^ j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)
                                                        else (match (res x, res y) with 
                                                              | (S i, S j) -> run rest ((S (i ^ j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                              | _ -> bad rest)
                                                        )
    
    | (And         :: rest, (x ::y ::s') :: frames) -> (if (flag > 0 && (x == (N var) || y == (N var))) then 
                                                        (match (res x, res y) with 
                                                          | (B i, B j) -> run rest ((B (i && j) :: s' ) :: frames) (insert var (B (i && j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)
                                                        else (match (res x, res y) with 
                                                              | (B i, B j) -> run rest ((B (i && j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                              | _ -> bad rest)
                                                      )
    
    | (Or          :: rest, (x ::y ::s') :: frames) -> (if (flag > 0 && (x == (N var) || y == (N var))) then 
                                                        (match (res x, res y) with 
                                                          | (B i, B j) -> run rest ((B (i || j) :: s' ) :: frames) (insert var (B (i || j)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)
                                                        else (match (res x, res y) with 
                                                              | (B i, B j) -> run rest ((B (i || j) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                              | _ -> bad rest)
                                                      )
    
    | (Not         :: rest, (x ::s')     :: frames) -> (
                                                          if (flag > 0 && x == (N var)) then
                                                        (
                                                          match (res x) with 
                                                          | (B i) -> run rest ((B (not i) :: s' ) :: frames) (insert var (B (not i)) env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest
                                                        )
                                                        else (
                                                          match (res x) with 
                                                          | (B i) -> run rest ((B (not i) :: s' ) :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest
                                                        )
                                                        )
    | (Equal       :: rest, (x ::y ::s') :: frames) -> (
                                                          match (res x, res y) with 
                                                          | (I i, I j) -> run rest ((B (i = j) :: s') :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)
    | (LessThan    :: rest, (x ::y ::s') :: frames) -> (match (res x, res y) with  
                                                          | (I i, I j) -> run rest ((B (i < j) :: s') :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)
                                          
    | (If          :: rest,(x::y::z::s') :: frames) -> (match res z with 
                                                          | B true -> run rest ((y :: s'):: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | B false -> run rest ((x :: s'):: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
                                                          | _ -> bad rest)

    | (Pop         :: rest, (_ :: s')    :: frames) -> run rest (s' :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
    
    | (Swap        :: rest,( x ::y ::s') :: frames) -> run rest (( y ::x ::s') :: frames) env func_env name1 var name2 num_stack flag num_stack1 func_map
    
    | (Bind        :: rest, ( N n::x::s'):: frames) -> (
                                                        match x with
                                                        | N x' -> ( if fetch_func x' func_env != None then
                                                                      match fetch_func x' func_env with
                                                                        | Some (t, (envir, c, n2)) -> let c' = (envir, c, n2) in 
                                                                                                      let func_env = bind_func n c' t func_env in
                                                                                                      let func_map = push_map n func_env func_map in
                                                                                                      run rest (insertStack U (s' :: frames)) envir func_env name1 var name2 num_stack flag num_stack1 func_map
                                                                    else match fetch x' env with
                                                                        | Some v ->  run rest ((U :: s'):: frames) (insert n v env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                                        | _ -> bad rest)
                                                        | _ -> match res x with
                                                                | v ->  run rest ((U :: s'):: frames) (insert n v env) func_env name1 var name2 num_stack flag num_stack1 func_map
                                                                | _ -> bad rest
                                                                  ) 

    | (Begin       :: rest, frames                ) ->
                                                      run rest ([] :: frames) (pushEnv env) (push_func_env func_env ) name1 var name2 num_stack flag num_stack1 func_map

    | (End         :: rest, ((top :: _) :: frames)) -> 
                                                        run rest (insertStack top frames) (popEnv env) (pop_func_env func_env ) name1 var name2 num_stack flag num_stack1 func_map


  
    | (Quit        :: _   , s :: _                ) -> (s)
    | ([]                 , s :: _                ) -> (s)
    
    | (_           :: rest, _                     ) -> bad rest

(* parser combinators over exploded strings *)
let explode (s:string) : char list =
let rec expl i l =
  if i < 0 
  then l 
  else expl (i - 1) (String.get s i :: l)
in expl (String.length s - 1) []

let implode (cl:char list) : string = 
String.concat "" (List.map (String.make 1) cl)

let is_alpha (c:char): bool = 
(Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z')
|| (Char.code 'A' <= Char.code c && Char.code c <= Char.code 'Z')

let is_digit (c:char): bool = 
Char.code '0' <= Char.code c && Char.code c <= Char.code '9'

let rec take_while' (p: 'a -> bool) (es : 'a list) : ('a list) * ('a list) = 
match es with
| []      -> ([],[])
| x :: xs -> if p x then let (chars, rest) = take_while' p xs in  (x :: chars, rest) else ([], x :: xs)

let take_while (p:char -> bool) (s:string) : string * string = 
let (echars, erest) = take_while' p (explode s) 
in (implode echars, implode erest)

let parse_int (s : string) : int option = 
  match int_of_string s with    
  | n -> Some n
  | exception _ -> None

let parse_string (s : string) : string option = 
  if String.length s > 1 && String.get s 0 = '"' && String.get s (String.length s - 1) = '"'
  then  Some (String.sub s 1 (String.length s - 2)) (* this is less restrictive then the spec *)
  else None

let parse_name (s : string) : string option = 
  if String.length s > 0 && ( let c = (String.get s 0) in is_alpha c ||  c = '_')
  then  Some s (* this is less restrictive then the spec *)
  else None
  
let parse_constant (s:string) : stackVal = 
  let s' = String.trim s in
  match s' with
  | "<true>"  -> B true
  | "<false>" -> B false
  | "<unit>"  -> U
  | _ -> match parse_int s' with
         | Some i -> I i
         | None -> match parse_string s' with
                   | Some s -> S s
                   | None -> match parse_name s' with
                             | Some s -> N s
                             | None -> E

let parse_single_command (s:string) : command = 
  match take_while is_alpha (String.trim s) with
  | ("PushI"   , p) -> PushI (parse_constant p)
  | ("PushS"   , p) -> PushS (parse_constant p)
  | ("PushN"   , p) -> PushN (parse_constant p)
  | ("PushB"   , p) -> PushB (parse_constant p)
  | ("Push"    , p) -> Push (parse_constant p)
  | ("Add"     , _) -> Add
  | ("Sub"     , _) -> Sub
  | ("Mul"     , _) -> Mul
  | ("Div"     , _) -> Div
  | ("Rem"     , _) -> Rem
  | ("Return"  , _) -> Return
  | ("Neg"     , _) -> Neg
  | ("Pop"     , _) -> Pop
  | ("Swap"    , _) -> Swap
  | ("Concat"  , _) -> Concat
  | ("And"     , _) -> And
  | ("Or"      , _) -> Or
  | ("Not"     , _) -> Not
  | ("LessThan", _) -> LessThan
  | ("Equal"   , _) -> Equal
  | ("If"      , _) -> If
  | ("Begin"   , _) -> Begin
  | ("End"     , _) -> End
  | ("Bind"    , _) -> Bind
  | ("Quit"    , _) -> Quit
  | ("Call"    , _) -> Call
  | ("Fun"     , p) -> (let sl = String.split_on_char ' ' p in
                        let n1 = List.nth sl 1 in
                        let n2 = List.nth sl 2 in
                        Fun (parse_constant n1, parse_constant n2))
  | ("InOutFun", p) -> (let sl = String.split_on_char ' ' p in
                        let n1 = List.nth sl 1 in
                        let n2 = List.nth sl 2 in
                        InOutFun (parse_constant n1, parse_constant n2))
  | ("FunEnd"  , _) -> FunEnd
  (* any unknown commands will result in an exception *)


(* file IO *)

(* from lab 3 *)
let rec read_lines (ch : in_channel) : string list =
match input_line ch with 
  | str                    -> str :: read_lines ch
  | exception  End_of_file -> [] (* input_line throws an exception at the end of the file *)
  
  
(* from lab 3 *)
let rec write_lines (ch) (ls : string list ) : unit =
match ls with
  | []      -> ()
  | x :: xs -> let _ = Printf.fprintf ch "%s\n" x in write_lines ch xs
  
(* run the interperter on the commands in inputFile and write the resulting stack in outputFile *)
let interpreter (inputFile : string) (outputFile : string) : unit =
let ic = open_in inputFile in
let lines_in = List.map String.trim (read_lines ic) in
let _ = close_in ic in

let commands = List.map  parse_single_command lines_in in
let length_stack = Stack.create() in 
let var_stack = Stack.create() in 
let num_stack1 = Stack.create() in
let stack = run commands [[]] empEnv [] "" "" var_stack length_stack 0 num_stack1 [] in
let lines_out = List.map to_string stack  in

let oc = open_out outputFile in
let _ = write_lines oc lines_out in
let _ = close_out oc in ()
;;
interpreter "input22.txt" "output";;
open Str
(*#load "str.cma"*)
let stack = Stack.create()
let mark = Stack.create()
(* 0: int, 
  1: bool, 
  2: string, 
  3: name, 
  4: error, 
  5: unit*)
type nameVal = 
  | I of int
  | S of string
  | B of bool
  | U 
let to_string (s : nameVal) : string = 
  match s with
  | I i -> string_of_int i 
  | S s  -> s
  | B b -> "<" ^ string_of_bool b ^ ">"
  | U   -> "<unit>"

let rec print_ls (ls : (string*nameVal) list) : string = 
  match ls with
  | (n, v) :: xs -> n ^ ", " ^ (to_string v)  ^ print_ls xs
  | [] -> " "
let rec ls2string (ls : (string * nameVal) list list) : string = 
  match ls with
  | hd :: rest -> (print_ls hd) ^"\n" ^ ls2string rest
  | [] -> " "

let rec fetch_helper (name :string)  (env: (string * nameVal)list) : nameVal option = 
  match env with
  | (name' , v) :: rest -> if (String.equal name' name) then Some v else fetch_helper name rest
  | [] -> None

let rec fetch (name:string) (env : (string*nameVal)list list) : nameVal option = 
  match env with
  | ls :: rest -> (match fetch_helper name ls with
                  | Some x -> Some x
                  | None -> fetch name rest) 
  | [] -> None


let check_str (s: string) : bool = 
  try int_of_string s |> ignore; true
  with Failure _ -> false
let check_string (s: string) : bool = 
  if (Char.equal (String.get s 0) '"' == true && Char.equal (String.get s (String.length s - 1)) '"' == true) then true
  else false
let idregex = Str.regexp "^\\(_*\\)\\([a-zA-Z]+\\)\\([a-zA-Z0-9_]*\\)$";;
let check_name (s: string) : bool = 
  if (Str.string_match idregex s 0) then true
  else false

let pushI (s : string) (stack : string Stack.t)(stack1 : int Stack.t) : unit = 
  if check_str s then 
            (if (String.equal "-0" s) then (Stack.push "0" stack; Stack.push 0 stack1)
            else (Stack.push s stack; Stack.push 0 stack1))
  else (Stack.push "<error>" stack; Stack.push 4 stack1)

let pushS (s : string) (stack : string Stack.t)(stack1 :int Stack.t) : unit =
  let _ = Stack.push s stack in
  Stack.push 2 stack1

let pushN (n : string) (stack : string Stack.t)(stack1 :int Stack.t) : unit = 
  if (check_name n) then (Stack.push n stack; Stack.push 3 stack1)
  else (Stack.push "<error>" stack; Stack.push 4 stack1)

let pushB (s: string) (stack : string Stack.t)(stack1 :int Stack.t) : unit =
  if (String.equal s "<true>" || String.equal s "<false>") then (Stack.push s stack; Stack.push 1 stack1)
  else (Stack.push "<error>" stack; Stack.push 4 stack1) 

let push (s : string) (stack : string Stack.t)(stack1 :int Stack.t) : unit = 
  if (String.equal s "<unit>") then (Stack.push "<unit>" stack; Stack.push 5 stack1)
  else if (String.equal s "<error>") then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (Stack.push "<error>" stack; Stack.push 4 stack1)

let pop (stack : string Stack.t)(stack1 :int Stack.t) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (let _ = Stack.pop stack1 in 
        let _ = Stack.pop stack in ())
let push2Back (s1:string)(s2 : string)(t1:int)(t2:int)(stack: string Stack.t) (stack1 : int Stack.t) : unit =
  Stack.push s2 stack;
  Stack.push s1 stack;
  Stack.push t2 stack1;
  Stack.push t1 stack1;
  Stack.push "<error>" stack;
  Stack.push 4 stack1
let push1Back (s1:string)(t1:int)(stack:string Stack.t)(stack1: int Stack.t) : unit = 
  Stack.push s1 stack;
  Stack.push t1 stack1;
  
  Stack.push "<error>" stack;
  Stack.push 4 stack1
let push3Back (x:string)(y:string)(z:string)(t1:int)(t2:int)(t3:int)(stack: string Stack.t)(stack1:int Stack.t) :unit = 
  Stack.push z stack;
  Stack.push y stack;
  Stack.push x stack;
  Stack.push t3 stack1;
  Stack.push t2 stack1;
  Stack.push t1 stack1;
  Stack.push "<error>" stack;
  Stack.push 4 stack1
  
let add (stack : string Stack.t)(stack1 :int Stack.t)(ls : (string*nameVal)list list) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack  in
      let s2 = Stack.pop stack  in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if t1 == 0 && t2 == 0 then (
        
        Stack.push 
            (string_of_int(int_of_string s1 + int_of_string s2)) stack; Stack.push 0 stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in
        (
          match x with
          | Some x -> (match x with 
                      | I i1 -> (match fetch s2 ls with 
                                | Some y -> (match y with 
                                            | I i2 -> (Stack.push (string_of_int (i1 + i2)) stack; Stack.push 0 stack1)
                                            | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                | None -> push2Back s1 s2 t1 t2 stack stack1)
                      | _ -> push2Back s1 s2 t1 t2 stack stack1)
          | None -> push2Back s1 s2 t1 t2 stack stack1
        )
      )
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I x -> (Stack.push (string_of_int (x + int_of_string s2)) stack; Stack.push 0 stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I x -> (Stack.push (string_of_int (int_of_string s1 + x)) stack; Stack.push 0 stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
    )
  )
let sub (stack : string Stack.t)(stack1 :int Stack.t)(ls : (string*nameVal)list list) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack;  Stack.push 4 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if check_str s1 && check_str s2 then (Stack.push 
        (string_of_int(int_of_string s1 - int_of_string s2)) stack; Stack.push 0 stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in
        (
          match x with
          | Some x -> (match x with 
                      | I i1 -> (let y = fetch s2 ls in 
                                  (
                                    match y with 
                                    | Some y -> (match y with 
                                                | I i2 -> (Stack.push (string_of_int (i1 - i2)) stack; Stack.push 0 stack1)
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    | None -> push2Back s1 s2 t1 t2 stack stack1
                                  )
                                )
                      | _ -> push2Back s1 s2 t1 t2 stack stack1
                      )
          | None -> push2Back s1 s2 t1 t2 stack stack1
        )
      )
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I x -> (Stack.push (string_of_int (x - int_of_string s2)) stack; Stack.push 0 stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I x -> (Stack.push (string_of_int (int_of_string s1 - x)) stack; Stack.push 0 stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
    )
  )
let mul (stack : string Stack.t)(stack1 :int Stack.t) (ls : (string*nameVal)list list): unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack;  Stack.push 4 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if check_str s1 && check_str s2 then (Stack.push 
        (string_of_int(int_of_string s1 * int_of_string s2)) stack; Stack.push 0 stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in
        (
          match x with
          | Some x -> (match x with 
                      | I i1 -> (let y = fetch s2 ls in 
                                  (
                                    match y with 
                                    | Some y -> (match y with 
                                                | I i2 -> (Stack.push (string_of_int (i1 * i2)) stack; Stack.push 0 stack1)
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    |None -> push2Back s1 s2 t1 t2 stack stack1
                                  )
                                )
                      | _ -> push2Back s1 s2 t1 t2 stack stack1
                      )
          | None -> push2Back s1 s2 t1 t2 stack stack1
        )
      )
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I x -> (Stack.push (string_of_int (x * int_of_string s2)) stack; Stack.push 0 stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I x -> (Stack.push (string_of_int (int_of_string s1 * x)) stack; Stack.push 0 stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
    )
  )
let div (stack : string Stack.t)(stack1 :int Stack.t)(ls:(string*nameVal)list list) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack;Stack.push 0 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if ( t1 == 0 && t2 == 0 && (int_of_string s2) != 0) then (Stack.push 
            (string_of_int(int_of_string s1 / int_of_string s2)) stack; Stack.push 0 stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in
        (
          match x with
          | Some x -> (match x with 
                      | I i1 -> (let y = fetch s2 ls in 
                                  (
                                    match y with 
                                    | Some y -> (match y with 
                                                | I i2 -> (
                                                  if (i2 != 0) then (Stack.push (string_of_int (i1 / i2)) stack; Stack.push 0 stack1)
                                                  else push2Back s1 s2 t1 t2 stack stack1)
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    |None -> push2Back s1 s2 t1 t2 stack stack1
                                  )
                                )
                      | _ -> push2Back s1 s2 t1 t2 stack stack1
                      )
          | None -> push2Back s1 s2 t1 t2 stack stack1
        )
      )
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I x -> (if (int_of_string s2)!= 0 then (Stack.push (string_of_int (x / int_of_string s2)) stack; Stack.push 0 stack1)
                              else push2Back s1 s2 t1 t2 stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I x -> (if x != 0 then (Stack.push (string_of_int (int_of_string s1 / x)) stack; Stack.push 0 stack1)
                              else push2Back s1 s2 t1 t2 stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
    )
  )

let rem (stack : string Stack.t) (stack1 :int Stack.t)(ls:(string*nameVal)list list): unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack;Stack.push 4 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if ( t1 == 0 && t2 == 0 && (int_of_string s2) != 0) then (Stack.push 
            (string_of_int(int_of_string s1 mod int_of_string s2)) stack; Stack.push 0 stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in
        (
          match x with
          | Some x -> (match x with 
                      | I i1 -> (let y = fetch s2 ls in 
                                  (
                                    match y with 
                                    | Some y -> (match y with 
                                                | I i2 -> (
                                                  if (i2 != 0) then (Stack.push (string_of_int (i1 mod i2)) stack; Stack.push 0 stack1)
                                                  else push2Back s1 s2 t1 t2 stack stack1)
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    | None -> push2Back s1 s2 t1 t2 stack stack1
                                  )
                                )
                      | _ -> push2Back s1 s2 t1 t2 stack stack1
                      )
          | None -> push2Back s1 s2 t1 t2 stack stack1
        )
      )
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I x -> (if (int_of_string s2)!= 0 then (Stack.push (string_of_int (x mod (int_of_string s2))) stack; Stack.push 0 stack1)
                              else push2Back s1 s2 t1 t2 stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I x -> (if x != 0 then (Stack.push (string_of_int ((int_of_string s1) mod x)) stack; Stack.push 0 stack1)
                              else push2Back s1 s2 t1 t2 stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
    )
  )
let equalOp (stack : string Stack.t)(stack1 :int Stack.t) (ls : (string*nameVal) list list): unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack  in
      let s2 = Stack.pop stack  in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in

      if t1 == 0 && t2 == 0 then (
        let i1 = int_of_string s1 in
        let i2 = int_of_string s2 in
        if (i1 == i2) then (
                      Stack.push "<true>" stack;
                      Stack.push 1 stack1
                      )
        else (
          Stack.push "<false>" stack;
          Stack.push 1 stack1
        )
      )
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in(
          match x with
          | Some x -> (match x with 
                      | I i1 -> (let y = fetch s2 ls in (
                                    match y with 
                                    | Some y -> (match y with 
                                                | I i2 -> (
                                                  if (i1 == i2) then (Stack.push "<true>" stack; Stack.push 1 stack1)
                                                  else (Stack.push "<false>" stack; Stack.push 1 stack1))
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    | None -> push2Back s1 s2 t1 t2 stack stack1))
                      | _ -> push2Back s1 s2 t1 t2 stack stack1)
          | None -> push2Back s1 s2 t1 t2 stack stack1))
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I x -> (if (int_of_string s2) == x then (Stack.push "<true>" stack; Stack.push 1 stack1)
                              else (Stack.push "<false>" stack; Stack.push 1 stack1))
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I x -> (if x == (int_of_string s1) then (Stack.push "<true>" stack; Stack.push 1 stack1)
                              else (Stack.push "<false>" stack; Stack.push 1 stack1))
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | None -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1)
  )
let lessOp (stack : string Stack.t)(stack1 :int Stack.t)(ls : (string*nameVal)list list) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack  in
      let s2 = Stack.pop stack  in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in

      if t1 == 0 && t2 == 0 then (
        let i1 = int_of_string s1 in
        let i2 = int_of_string s2 in
        if (i1 < i2) then (
          Stack.push "<true>" stack;
          Stack.push 1 stack1
        )
        else (
          Stack.push "<false>" stack;
          Stack.push 1 stack1
        )
      )
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in(
          match x with
          | Some x -> (match x with 
                      | I i1 -> (let y = fetch s2 ls in (
                                    match y with 
                                    | Some y -> (match y with 
                                                | I i2 -> (if (i1 < i2) then (Stack.push "<true>" stack; Stack.push 1 stack1)
                                                           else (Stack.push "<false>" stack; Stack.push 1 stack1))
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    | None -> push2Back s1 s2 t1 t2 stack stack1))
                      | _ -> push2Back s1 s2 t1 t2 stack stack1)
          | None -> push2Back s1 s2 t1 t2 stack stack1))
      else if (t1 == 3 && t2 == 0) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | I i1 -> (if i1 < (int_of_string s2) then (Stack.push "<true>" stack; Stack.push 1 stack1)
                              else (Stack.push "<false>" stack; Stack.push 1 stack1))
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 0 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | I i1 -> (if (int_of_string s1) < i1 then (Stack.push "<true>" stack; Stack.push 1 stack1)
                              else (Stack.push "<false>" stack; Stack.push 1 stack1))
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
    )
  )
let ifOp (stack: string Stack.t)(stack1:int Stack.t)(ls : (string*nameVal)list list) : unit = 
  if (Stack.length stack < 3) then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (
    let x = Stack.pop stack  in
    let y = Stack.pop stack  in
    let z = Stack.pop stack in
    let t1 = Stack.pop stack1 in
    let t2 = Stack.pop stack1 in
    let t3 = Stack.pop stack1 in

    if (t3 == 1) then (
      if (String.equal z "<true>") then (
      Stack.push y stack;
      Stack.push t2 stack1
      )
      else(
        Stack.push x stack;
        Stack.push t1 stack1
      ))
    else if (t3 == 3) then (
      match fetch z ls with
      | Some v -> (match v with
                  | B v -> (if v then (Stack.push y stack; Stack.push t2 stack1)
                            else (Stack.push x stack; Stack.push t1 stack1))
                  | _ -> push3Back x y z t1 t2 t3 stack stack1)
      | None -> push3Back x y z t1 t2 t3 stack stack1
    )
    else push3Back x y z t1 t2 t3 stack stack1
  )
let neg (stack: string Stack.t)(stack1 :int Stack.t)(ls : (string*nameVal)list list) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (
    let s1 = Stack.pop stack in
    let t1 = Stack.pop stack1 in
    if t1 == 0 then 
        (Stack.push (string_of_int(0 - int_of_string s1)) stack; Stack.push 0 stack1)
    else if t1 == 3 then
        (match (fetch s1 ls) with
        | Some v -> (match v with
                    | I x -> (Stack.push (string_of_int(0 - x)) stack; Stack.push 0 stack1)
                    | _ -> push1Back s1 t1 stack stack1)
        | None -> push1Back s1 t1 stack stack1)
    else push1Back s1 t1 stack stack1
  )

let swap (stack : string Stack.t)(stack1 :int Stack.t) : unit = 
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 0 stack1)
  else (
    if Stack.length stack == 1 then (Stack.push "<error>" stack; Stack.push 0 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      (Stack.push s1 stack;
      Stack.push s2 stack;
      Stack.push t1 stack1;
      Stack.push t2 stack1)
    )
  )
let concat (stack: string Stack.t)(stack1: int Stack.t)(ls : (string*nameVal)list list):unit=
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 4 stack1)
  else (
    if Stack.length stack < 2 then (Stack.push "<error>" stack; Stack.push 4 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if (t1 == 2 && t2 == 2) then (let res = s1 ^ s2 in pushS res stack stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in(
          match x with
          | Some x -> (match x with 
                      | S i1 -> (let y = fetch s2 ls in (
                                    match y with 
                                    | Some y -> (match y with 
                                                | S i2 -> (pushS (i1^i2) stack stack1; )
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    |_ -> push2Back s1 s2 t1 t2 stack stack1))
                      | _ -> push2Back s1 s2 t1 t2 stack stack1)
          | _ -> push2Back s1 s2 t1 t2 stack stack1))
      else if (t1 == 3 && t2 == 2) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | S x -> pushS (x ^ s2) stack stack1
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 2 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | S x -> pushS (s1 ^ x) stack stack1
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else push2Back s1 s2 t1 t2 stack stack1
      )
    )
  
let andOp (stack: string Stack.t)(stack1: int Stack.t)(ls : (string*nameVal)list list):unit=
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 0 stack1)
  else (
    if Stack.length stack < 2 then (Stack.push "<error>" stack; Stack.push 0 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if (t1 == 1 && t2 == 1) then (
        let s1 = String.sub s1 1 (String.length s1 - 2) in
        let s2 = String.sub s2 1 (String.length s2 - 2) in
        let b1 = bool_of_string s1 in
        let b2 = bool_of_string s2 in
        let res = b1 && b2 in 
        let s = "<"^(string_of_bool res)^">" in
        pushB s stack stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in(
          match x with
          | Some x -> (match x with 
                      | B i1 -> (let y = fetch s2 ls in (
                                    match y with 
                                    | Some y -> (match y with 
                                                | B i2 -> (pushB ("<"^(string_of_bool(i1 && i2))^">") stack stack1)
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    |_ -> push2Back s1 s2 t1 t2 stack stack1))
                      | _ -> push2Back s1 s2 t1 t2 stack stack1)
          | _ -> push2Back s1 s2 t1 t2 stack stack1))
      else if (t1 == 3 && t2 == 1) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | B x ->  (pushB ("<"^(string_of_bool(x && (bool_of_string (String.sub s2 1 (String.length s2 - 2)))))^">") stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 1 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | B x -> (pushB ("<"^(string_of_bool(x && (bool_of_string (String.sub s1 1 (String.length s1 - 2)))))^">") stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else (push2Back s1 s2 t1 t2 stack stack1
      )
    )
  )
let orOp (stack: string Stack.t)(stack1: int Stack.t)(ls:(string*nameVal)list list):unit=
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 0 stack1)
  else (
    if Stack.length stack < 2 then (Stack.push "<error>" stack; Stack.push 0 stack1)
    else (
      let s1 = Stack.pop stack in
      let s2 = Stack.pop stack in
      
      let t1 = Stack.pop stack1 in
      let t2 = Stack.pop stack1 in
      if (t1 == 1 && t2 == 1) then (
        let s1 = String.sub s1 1 (String.length s1 - 2) in
        let s2 = String.sub s2 1 (String.length s2 - 2) in
        let b1 = bool_of_string s1 in
        let b2 = bool_of_string s2 in
        let res = b1 || b2 in 
        let s = "<"^(string_of_bool res)^">" in
        pushB s stack stack1)
      else if (t1 == 3 && t2 == 3) then (
        let x = fetch s1 ls in(
          match x with
          | Some x -> (match x with 
                      | B i1 -> (let y = fetch s2 ls in (
                                    match y with 
                                    | Some y -> (match y with 
                                                | B i2 -> (pushB ("<"^(string_of_bool(i1 || i2))^">") stack stack1)
                                                | _ -> push2Back s1 s2 t1 t2 stack stack1)
                                    |_ -> push2Back s1 s2 t1 t2 stack stack1))
                      | _ -> push2Back s1 s2 t1 t2 stack stack1)
          | _ -> push2Back s1 s2 t1 t2 stack stack1))
      else if (t1 == 3 && t2 == 1) then (
        match fetch s1 ls with
        | Some x -> (match x with
                    | B x ->  (pushB ("<"^(string_of_bool(x || (bool_of_string (String.sub s2 1 (String.length s2 - 2)))))^">") stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else if (t1 == 1 && t2 == 3) then (
        match fetch s2 ls with
        | Some x -> (match x with
                    | B x -> (pushB ("<"^(string_of_bool(x || (bool_of_string (String.sub s1 1 (String.length s1 - 2)))))^">") stack stack1)
                    | _ -> push2Back s1 s2 t1 t2 stack stack1)
        | _ -> push2Back s1 s2 t1 t2 stack stack1
      )
      else (push2Back s1 s2 t1 t2 stack stack1
      )
    )
  )

let rec update_helper (name:string)(v: nameVal)(ls : (string*nameVal)list) : (string*nameVal)list = 
  match ls with
  | (name', v') :: rest ->(if (String.equal name' name) then (name' ,v ):: rest
                            else (name', v'):: (update_helper name v rest))
  | [] -> ls
let rec update (name:string)(v:nameVal)(env:(string*nameVal)list list) : (string*nameVal)list list = 
  match env with
  | ls :: rest -> (match fetch_helper name ls with
                  | Some x -> (update_helper name v ls) :: rest 
                  | None -> update name v rest)
  | _ -> env

let notOp (stack: string Stack.t)(stack1: int Stack.t)(ls : (string*nameVal)list list):unit =
  if Stack.is_empty stack then (Stack.push "<error>" stack; Stack.push 0 stack1)
  else (
      let s1 = Stack.pop stack in      
      let t1 = Stack.pop stack1 in

      if (t1 == 1) then (
        let s1 = String.sub s1 1 (String.length s1 - 2) in
        let b1 = bool_of_string s1 in
        let res = not b1 in 
        let s = "<"^(string_of_bool res)^">" in
        pushB s stack stack1)
      else if (t1 == 3) then (
        match fetch s1 ls with
        | Some v -> match v with
                    | B b -> match b with 
                            | true -> (Stack.push "<false>" stack; Stack.push 1 stack1)
                            | false -> (Stack.push "<true>" stack; Stack.push 1 stack1)
                    | _ -> (push1Back s1 t1 stack stack1)
        | _ -> (push1Back s1 t1 stack stack1))
      else (push1Back s1 t1 stack stack1))

let string2nameVal (value : string) (t : int) : nameVal = 
  if (t == 0) then I (int_of_string value)
  else if (t == 1) then (
    if (String.equal value "<true>") then B true
    else B false
  )
  else if (t == 2) then S value
  else U


let add_ele (name:string)(v:nameVal)(ls : (string*nameVal)list) : (string*nameVal)list = 
  match v with
  | I i -> (name, I i) :: ls
  | B b -> (name, B b) :: ls 
  | S s -> (name, S s) :: ls
  | U -> (name, U) :: ls

let bind_val (name: string)(v : nameVal)(ls: (string * nameVal) list list) : (string*nameVal) list list= 
  match ls with
  | xs :: rest -> (match fetch_helper name xs with
                    | Some res -> update name v ls
                    | None -> (add_ele name v xs) :: rest)
  | _ ->  (add_ele name v []) :: []

let bindOp (stack: string Stack.t)(stack1 : int Stack.t) (ls: (string * nameVal) list list): (string * nameVal) list list = 
  if (Stack.length stack < 2) then (Stack.push "<error>" stack; let _ = Stack.push 4 stack1 in ls)
  else(
    let x = Stack.pop stack in
    let y = Stack.pop stack in
    let t1 = Stack.pop stack1 in
    let t2 = Stack.pop stack1 in
    if (t1 != 3 || (t2 != 0 && t2 !=1 && t2 != 2 && t2 != 3 && t2 != 5)) then (
      let _ = push2Back x y t1 t2 stack stack1 in ls
    )
    else (
      
      
      if (t2 == 3) then (match (fetch y ls) with 
                        | Some v -> (Stack.push "<unit>" stack;Stack.push 5 stack1;bind_val x v ls)
                        | None -> (let _ = push2Back x y t1 t2 stack stack1 in ls)
                      )
      else (
        Stack.push "<unit>" stack;Stack.push 5 stack1;
        let res = string2nameVal y t2 in
        bind_val x res ls;
      )
    )
  )

let write_output (filename : string) (stack : string Stack.t) : unit = 
let oc = open_out filename in
while Stack.is_empty stack != true do
  let line = Stack.pop stack in
  Printf.fprintf oc "%s\n" line;
done;
close_out oc

let quit (stack: string Stack.t)(filename : string):unit= 
  write_output filename stack

let read_line (inc : in_channel) : string option = 
  match input_line inc with
  | l -> Some l
  | exception End_of_file -> None;;

let read_input (filename: string) : string list = 
  let rec readlines (ch : in_channel) : string list =
    match input_line ch with 
      | str -> str :: readlines ch
      | exception  End_of_file -> [] (* input_line throws an exception at the end of the file *)
  in let ch = open_in filename in 
  readlines ch
let rec pop_helper (n:int)(stack:string Stack.t)(stack1:int Stack.t) : unit = 
  if (n == 0) then ()
  else (Stack.pop stack; Stack.pop stack1; pop_helper (n-1) stack stack1)

let remove (ls : (string*nameVal)list list )(stack: string Stack.t)(stack1:int Stack.t)(n : int) : (string*nameVal)list list =
  match ls with
  | xs :: rest -> (if(n==1) then rest 
                  else (let temp = Stack.pop stack in
                        let t = Stack.pop stack1 in
                        pop_helper (n-1) stack stack1;
                        Stack.push temp stack;
                        Stack.push t stack1;
                        rest))
  | [] -> []
let rec process (ls : string list)(stack: string Stack.t)(stack1 : int Stack.t)(filename : string)(ns : (string*nameVal) list list )(num: int Stack.t) : (string * nameVal) list list = 
  match ls with
  | [] -> let _ = quit stack filename in ns
  | hd :: xs -> (let sl = String.split_on_char ' ' hd in
                  if List.length sl == 1 then (
                    let s = List.hd sl in (
                      match s with
                      | "Quit" -> quit stack filename; ns;
                      | "Swap" -> swap stack stack1;process xs stack stack1 filename ns num;
                      | "Neg" -> neg stack stack1 ns; process xs stack stack1 filename ns num
                      | "Add" -> add stack stack1 ns; process xs stack stack1 filename ns num
                      | "Sub" -> sub stack stack1 ns; process xs stack stack1 filename ns num
                      | "Mul" -> mul stack stack1 ns; process xs stack stack1 filename ns num
                      | "Div" -> div stack stack1 ns; process xs stack stack1 filename ns num
                      | "Rem" -> rem stack stack1 ns; process xs stack stack1 filename ns num
                      | "Concat" -> concat stack stack1 ns; process xs stack stack1 filename ns num
                      | "And" -> andOp stack stack1 ns; process xs stack stack1 filename ns num
                      | "Or" -> orOp stack stack1 ns; process xs stack stack1 filename ns num
                      | "Not" -> notOp stack stack1 ns; process xs stack stack1 filename ns num
                      | "Equal" -> equalOp stack stack1 ns; process xs stack stack1 filename ns num
                      | "LessThan" -> lessOp stack stack1 ns; process xs stack stack1 filename ns num
                      | "Bind" ->process xs stack stack1 filename (bindOp stack stack1 ns) num
                      | "Pop" -> pop stack stack1; process xs stack stack1 filename ns num
                      | "If" -> ifOp stack stack1 ns; process xs stack stack1 filename ns num   
                      | "Begin" -> (let ns = [] :: ns in 
                                    let n = Stack.length stack in 
                                    let _ = Stack.push n num in
                                    process xs stack stack1 filename ns num)
                      | "End" -> (let n2 = Stack.length stack in
                                  let len = n2 - (Stack.pop num) in  
                                  let ns = remove ns stack stack1 len in process xs stack stack1 filename ns num)
                      | _ -> ns)
                  )
                  else if List.length sl == 2 then (
                    let s1 = List.hd sl in
                    let s2  = List.nth sl 1 in(
                      match s1 with
                      | "PushB" -> pushB s2 stack stack1; process xs stack stack1 filename ns num
                      | "PushI" -> pushI s2 stack stack1; process xs stack stack1 filename ns num
                      | "PushS" -> (if (Char.equal '"' (String.get s2 0) && Char.equal '"' (String.get s2 (String.length s2 - 1))) then 
                                      ( let res = String.sub s2 1 (String.length s2 - 2) in
                                        let _ = Stack.push res stack in
                                        let _ =Stack.push 2 stack1 in
                                      process xs stack stack1 filename ns num)
                                    else (let _ =Stack.push "<error>" stack in 
                                          let _ =Stack.push 4 stack1 in
                                          process xs stack stack1 filename ns num))
                      | "PushN" -> pushN s2 stack stack1; process xs stack stack1 filename ns num
                      | "Push" -> push s2 stack stack1; process xs stack stack1 filename ns num
                      | _ -> ns))
                  else (
                    let s1 = List.hd sl in
                    match s1 with
                    | "PushS" -> (if (Char.equal '"' (String.get hd 6) && Char.equal '"' (String.get hd (String.length hd - 1))) then (
                                    let st = List.tl sl in
                                    let rec recover (ls : string list): string = 
                                      (
                                        match ls with
                                        | [] -> ""
                                        | hd :: xs -> hd ^" "^ (recover xs)
                                      )in 
                                      let s2 = recover st in
                                      let res = String.sub s2 1 (String.length s2 - 3) in
                                      let _ =pushS res stack stack1 in 
                                      process xs stack stack1 filename ns num
                                  )
                                  else (
                                    Stack.push "<error>" stack;
                                    Stack.push 4 stack1;
                                    process xs stack stack1 filename ns num
                                  ))
                    | _ ->(
                            Stack.push "<error>" stack;
                            Stack.push 4 stack1;
                            process xs stack stack1 filename ns num
                          )
                    
                  )
  )
let interpreter (input : string) (output : string) : unit =
  let env = [] in
  let num_stack = Stack.create() in 
  let ls = read_input input in
  let env = process ls stack mark output env num_stack in ()
;;

(*interpreter "input" "output"*)
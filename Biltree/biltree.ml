open List
open Printf
open Char
(*  define a function write_list_int: string -> int list -> unit
 *  that takes the name of a file, a list of integers and writes
 *  all the elements of the list in the file, from left to right, 
 *  one per line, and then returns unit.
 *  Remember to close any possible channel before ending.
 *)
let write_list_int (s: string) (l: int list): unit =
  (* please replace the expression () below with your code *)
  let oc = open_out s in
  (*
  let rec write_line (ch) (l): unit =
  (
    match l with
    | [] -> ()
    | x :: xs -> 
               let line = string_of_int x in
               let () = print_string (line ^ "\n") in
               let _ = Printf.fprintf ch "%s\n" line in write_line ch xs 
  ) in 
  let _ = write_line ch l in
  let _ = close_out ch in () *)
  let printstr str = 
    fprintf oc "%s\n" (string_of_int str) in
    List.iter printstr l;
    close_out oc
  



(*  define a function read_list_int_rev: string -> int list
 *  that takes the name of a text file (which contains one integer per line) 
 *  and returns a list of all the integers in reversed order. That is 
 *  if the file looks like this: 
 *  1
 *  2
 *  3
 *  the returned list has to be [3;2;1] .
 *  Finally, remember to close any input channel before ending.
 *) 
let read_list_int_rev (f: string): (int list) = 
  (* please replace the expression [] below with your code *)
  let ch = open_in f in 
  let rec read_line (ch) : (int list) = 
    match input_line ch with
    | str -> int_of_string str :: read_line ch 
    | exception End_of_file -> []
  in 
  let l = read_line ch in
  let _ = close_in ch in List.rev l

(*let l = read_list_int_rev "hello.txt" in write_list_int "test.txt" l*)

(* Definition of biltrees *)
type biltree = B of bool   | I of int   | L of int list | T of biltree * char  * biltree

(* Some examples of biltrees *)
let ex1 = T (T (B true, 'a' , T (I (-34), 'b', L [-21; 53; 12])), 'c', T (I (-18), 'd' , B true))
let ex2 = T (T (T (T (I 31, 'h', L [9; 34; -45]), 'e', L [70; 58; -36; 28]), 'l', I 3), 'l', T (I 2, 'o', I 49))
let ex3 = T (T (T (L [9; 4; -1; 0; -5], 'c', B true), 's', B true), '3', T (B false, '2', I (-3)))


(* Functions you need to define *)
        
let rec count_nodes (p: biltree):int=
  (* please replace the 0 below with your code *)
 match p with 
 | B b -> 1
 | I i -> 1
 | L l -> 1
 | T (tl, c, tr)-> count_nodes tl + count_nodes tr + 1

let rec global_and (p: biltree): bool option =
  (* please replace the None below with your code *)
  let andfunc (a: bool option) (b: bool option) : bool option = 
    match a with 
    | None ->( match b with 
              | None -> None
              | Some b -> Some b)
    | Some a ->  match b with 
              | None -> Some a
              | Some b -> Some (a && b)
  in
  match p with 
  | B b -> Some b
  | I i -> None
  | L l -> None
  | T (tl, c, tr) -> andfunc (global_and tl) (global_and tr)

  
let rec sum_lists (p: biltree) : biltree =
  (* please replace the expression B true below with your code *)
  let rec sum_list (l : int list) : int = 
    match l with
    | [] -> 0
    | x :: xs -> x + sum_list xs
  in
  match p with
  | L l -> I (sum_list l)
  | T (tl, c, tr) -> T(sum_lists tl, c, sum_lists tr)
  | B b -> B b
  | I i -> I i


let rec f_on_all_ints (f : int -> int) (b: biltree): biltree =
  (* please replace the expression B true below with your code *)
  match b with
  | B b -> B b
  | L l -> L(List.map f l)
  | I i -> I (f i)
  | T (tl, c, tr) -> T (f_on_all_ints f tl, c, f_on_all_ints f tr)

let rec tostring_mlr (b : biltree): string =
  (* please replace the empty string "" below with your code *)
  let rec string_of_intlist (l : int list) : string = 
    match l with
    | [] -> ""
    | x :: xs -> string_of_int x ^ string_of_intlist xs
  in
 match b with
 | B b -> string_of_bool b
 | I i -> string_of_int i
 | L l -> string_of_intlist l
 | T (tl, c, tr) -> (Char.escaped c) ^ (tostring_mlr tl) ^ (tostring_mlr tr)


let queue = Queue.create();; 
let result = Queue.create();; 
let read_line (inc : in_channel) : string option = 
  match input_line inc with
  | l -> Some l
  | exception End_of_file -> None;;

let read_input (filename : string) : unit = 
  let rec read_all_lines (inc : in_channel) : unit = 
    let line = input_line inc;
    in Queue.add (String.lowercase_ascii line) queue;
    read_all_lines inc
    in 
    let in_file = open_in filename in 
      try 
        read_all_lines in_file 
      with End_of_file -> close_in in_file;; 

let update_alpha (start : int)(finish : int)(alpha : bool array )(str : string) : unit = 
  let rec loop (i : int) : unit = 
    if i > finish  || i = finish then ()
    else
      let index = Char.code (String.get str i) in
      if index < 123 && index > 96 then (
        alpha.(index - 97) <- true;
        loop (i + 1); )
      else loop (i + 1);
  in loop start;;

let check (start : int) (finish : int)(alpha : bool array) : bool = 
  let rec loop (i : int) : bool = 
    if i < finish then (if alpha.(i) = false then false else loop (i + 1))
    else true
  in loop start;;

let check_pangram (str : string) : bool =
  let str = String.trim str in
  if String.length str < 26 
    then false
  else 
    let alpha_mark_array = Array.init 26 (fun i -> false) in
    let () = update_alpha 0 (String.length str) alpha_mark_array str
    in check 0 (Array.length alpha_mark_array) alpha_mark_array;;

let write_output (filename : string) : unit = 
  let oc = open_out filename in 
  while Queue.is_empty result != true do 
    let line = string_of_bool (Queue.take result) in
    Printf.fprintf oc "%s\n" line;
  done;
  close_out oc;;

type file = string * string;;

let pangram (file_tuple : file) : unit = 
  let (input ,output) = file_tuple in 
  read_input input;
  while Queue.is_empty queue != true do
    let line = Queue.take queue in
    Queue.add(check_pangram line) result;
  done;
  write_output output;;
(*pangram ("input1.txt", "output1.txt");;
pangram ("input2.txt", "output2.txt");;
pangram ("input3.txt", "output3.txt");;
pangram ("input4.txt", "output4.txt");;
pangram ("input5.txt", "output5.txt");;*)
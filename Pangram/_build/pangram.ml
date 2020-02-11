let buf = Queue.create();; 

let catfile filename = 
  let rec print_all_lines in_chan = 
    Queue.add (input_line in_chan) buf; 
    print_all_lines in_chan 
    in 
    let in_file = open_in filename in 
      try 
        print_all_lines in_file 
      with End_of_file -> close_in in_file;; 

catfile "test.txt";; 

let read_line (inc : in_channel) : string option = 
  match input_line inc with
  | l -> Some l
  | exception End_of_file -> None;;

(*
let pangram (input : string) (output : string) : unit result = 
*)  
  
  
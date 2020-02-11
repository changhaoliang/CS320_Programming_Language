(* take attendance *)

(*
consider the grammar
exp ::=  ( <exp> ) | <exp> || <exp> | <exp> && <exp> | <val>
val  ::= true | false
*)


(*
give some strings that can be produced by this grammar
*)


(*
give some strings that cannot be produced by this grammar
*)

(* optional: the grammar can be modeled by data types that represent the parse trees *)


(* give the parse tree for the examples you gave *)


(* optional:  give a function that generates the string from the grammar *)


(*
is the grammar ambigous?
if so give an example string with 2 parse tress and explain how can we can disambiguate the grammar.

optional:  this new grammar can be modeled by data types that represent the parse trees 
*)
















(* if time: show the parse trees for the origional given examples *)

(* optional:  write a function that flattens out the term to a sting *)




(* the standard parsing architecture looks like
tokenize -> parse tree -> abstract syntax tree
-> evaluation
 *)
	
(* write a data type for tokens *)
(* type token *)

(* for evaluation we are not intrested in parentisis, we are only intresed in an unambigous tree of opertations
write an ast  *)
(* type ast = *)

(* write a parser that goes from a list of tokens to the Ast using the parse tree to guide your recursion  *)


	


(* if time: write a function that evalueates the ast *)


(* if time: write a tokenizer (hint: first write a program that goes from string -> char list)*)

(* let explode (s :string) : char list = *)

(* let tokenize (s:string) = *)


(* if time: put it all together *)


(* optional:  if time write a function that takes a list of tokens into a parse tree *)



(* optional:  if time write a function that takes a parse tree into an Ast *)

  


(* optional: put it all together again *)




(*
also check out the grammar for sml (a simplified version of ocaml) https://people.mpi-sws.org/~rossberg/sml.html
*)
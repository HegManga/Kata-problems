(*
First time i wrote "Tiny Three-pass compiler.ml" I made mistakes with grammar:
- correct "pass1" -> Compiler.pass1 "[x y z] x + y + z" = Add(Arg(0),Add(Arg(1),Arg(2))
- my "pass1" -> Compiler.pass1 "[x y z] x + y + z" = Add(Arg(0),Add(Arg(1),Arg(2))
I misinterpreted grammar and the order of derivation is changed.
Maybe my code is still useful changing the grammar. :) :)
*)
let tokenize code =
  let rec explode string =
    if String.length string = 0 then []
    else [String.sub string 0 1] @
           explode (String.sub string 1 ((String.length string) - 1))
  in
  let specialChars =
    [
      "["; "]"; "-"; "+"; "*"; "/"; "("; ")"
    ]
  in
  let nonSpecialHelper = function
    | "" -> []
    | str -> [str]
  in
  let rec tokenizeHelper = function
    | [],currentItem, tokens ->
       tokens @ (nonSpecialHelper currentItem)
    | " "::lst, currentItem, tokens ->
       tokenizeHelper(
           lst,"",
           tokens @ nonSpecialHelper currentItem)
    | item::lst, currentItem, tokens ->
       if List.mem item specialChars then
         tokenizeHelper(
             lst, "",
             tokens @ nonSpecialHelper currentItem @ [item])
       else
         tokenizeHelper(lst, currentItem ^ item,tokens)
  in
  tokenizeHelper(explode code, "", [])

type ast =
  | Imm of int  (* immediate value *)
  | Arg of int  (* reference to n-th argument *)
  | Add of (ast * ast) (* add first to second *)
  | Sub of (ast * ast) (* subtract second from first *)
  | Mul of (ast * ast) (* multiply first by second *)
  | Div of (ast * ast) (* divide first by second *)


  
exception CompilerError of string

module type COMPILER =
  sig
    val pass1: string -> ast
    val pass2: ast -> ast
    val codeGen: ast -> string list
    val compile: string -> string list 
  end


module Compiler : COMPILER =
  struct
     
    exception Noexp of string

let is_Imm str = 
	let rec aux s c =
		let ls =  String.length s 
		in
			if ls <0 then true
			else 
				if (Char.code c) >= (Char.code '0') && (Char.code c) <= (Char.code '9') then 
					if ls = 0 then true
					else aux (String.sub s 1 ((String.length s)-1)) (String.get s 0) 
				else false
	in 	
		if String.length str <=0 then false else aux str '0'

let is_Arg str argl = List.mem str argl


let split_codelist lc = 
	let rec aux l ((arg,exp) as ret) = 
		match l with
			|[]-> ret
			|h::t -> 
				if h = "]" then (arg,t)
				else aux t (h::arg,exp)
	and aux1 (x,y) = (List.rev x,y)
	in
		aux1 (aux (List.tl lc) ([],[]))


let listnth l x=
	let rec aux ll xx n =
		match ll with
			|[] -> -1
			|h::t when h = xx -> n
			|h::t -> aux t xx (n+1)
	in 
		aux l x 0

exception Imposs of string

let rec aux_ast argl expl expr =
	match expr with
		|[] -> raise (Imposs("Not possible aux_ast? []"))
		|[x] when is_Imm x -> 
			if expl = [] then Imm((int_of_string x))
			else aux_ast argl [] (expl@["(";x;")"])	
		|[x] when is_Arg x argl -> 
			if expl = [] then Arg((listnth argl x))
			else aux_ast argl [] (expl@["(";x;")"]) 
		|h::t when h = "+" -> Add( aux_ast argl [] expl , aux_ast argl [] t ) 
		|h::t when h = "-" -> Sub( aux_ast argl [] expl , aux_ast argl [] t )
		|f1::op::f2::[] when op = "*"  || op = "/" ->
			let op_aux ((x,y)) = function
				|"*" -> Mul(x,y)
				|"/" -> Div(x,y)
				|_ ->  raise (Imposs("Not possible op_aux"))
			in 
				let muldiv_cons argl1 expl1 expr1 =
					match expr1 with
						|f1::op::t when op = "*" -> Mul( aux_ast argl [] [f1] , aux_ast argl [] t ) 
						|f1::op::t when op = "/" -> Div( aux_ast argl [] [f1] , aux_ast argl [] t ) 
						|_ ->  raise (Imposs("Not possible muldiv_ cons"))
				in
					if expl = [] then op_aux ( aux_ast argl [] ([f1]) , aux_ast argl [] ([f2]) ) op
					else muldiv_cons argl [] (expl@expr)	
		|h::t when h = "(" -> 
			let rec aux_b left right i =
				match right with
					|h::t when h = "(" -> aux_b (left@[h]) t (i+1)
					|h::t when h = ")" -> 
						if i = 0 then (left@[h],t)
						else aux_b (left@[h]) t (i-1)
					|h::t -> aux_b (left@[h]) t i
					|_ -> raise (Imposs("Not possible aux_b"))
			in 
				let (bl,br) = aux_b [] expr (-1)

				in
					if br = [] then (
						if expl = [] then aux_ast argl [] (List.tl (List.rev (List.tl (List.rev bl))))
						else  
							let rec muldiv_consbrackets argl1 expl1 expr1 =
								match expr1 with
								|h::t when h = "(" -> 
									let (bl1,br1) = aux_b [] expr1 (-1)
									in
										muldiv_consbrackets argl1 bl1 br1
								|h::t when h = "*" -> Mul( aux_ast argl1 [] expl1 , aux_ast argl1 [] t ) 
								|h::t when h = "/" -> Div( aux_ast argl1 [] expl1 , aux_ast argl1 [] t ) 
								|_ ->  raise (Imposs("Not possible muldiv_ conbrackets"))		
							in
								muldiv_consbrackets argl [] (expl@bl)
						)
					else 	(
						if expl = [] then aux_ast argl bl br
						else aux_ast argl (expl@bl) br
						)		
		|h::t when h = "*" || h = "/" -> aux_ast argl (expl@[h]) t 
		|h::t -> aux_ast argl (expl@[h]) t
		
	
let pass1 code =
	let (argl,exp) = split_codelist (tokenize code) 
	in
		aux_ast argl [] exp


let is_Immast ast =
		match ast with 
			|Imm(x) -> true
			|_ -> false

let rec pass2 ast =
	let aux x y op =
		let f1 = pass2 x and f2 = pass2 y
		in
		if (is_Immast f1) && (is_Immast f2) then
			let op_aux1 (Imm(a)) (Imm(b)) = function
				|'+' -> Imm((a+b))
				|'-' -> Imm((a-b))
				|'*' -> Imm((a*b))
				|'/' -> Imm((a/b))
				| _  -> raise (Imposs("Not possible op_aux1 in pass2"))
			in op_aux1 f1 f2 op
		else 
			let op_aux2 a b = function
				|'+' -> Add(a,b)
				|'-' -> Sub(a,b)
				|'*' -> Mul(a,b)
				|'/' -> Div(a,b)
				| _  -> raise (Imposs("Not possible op_aux2 in pass2"))
			in op_aux2 f1 f2 op
	in
		match ast with
			|Add(x,y) -> aux x y '+'
			|Sub(x,y) -> aux x y '-'
			|Mul(x,y) -> aux x y '*'
			|Div(x,y) -> aux x y '/'
			|t -> t
      
    let rec codeGen ast =
	    match ast with
	    	|Imm(x) -> [Printf.sprintf "IM %d" x]
	    	|Arg(x) -> [Printf.sprintf "AR %d" x]
		    |Add(x,y) -> (codeGen y)@["PU"]@(codeGen x)@["SW";"PO";"AD"]
		    |Sub(x,y) -> (codeGen y)@["PU"]@(codeGen x)@["SW";"PO";"SW";"SU"]
		    |Mul(x,y) -> (codeGen y)@["PU"]@(codeGen x)@["SW";"PO";"MU"]
		    |Div(x,y) -> (codeGen y)@["PU"]@(codeGen x)@["SW";"PO";"SW";"DI"]
	
    let compile code =
      codeGen(pass2(pass1 code))
      
  end

let rec simualte : string list * int list -> int =
  let stack = Stack.create () in
  let r0 = ref 0 in
  let r1 = ref 0 in
  function
  | ([],argumets) -> !r0
  | ("SU"::lst,argumets) ->
     r0 := !r0 - !r1;
     simualte(lst,argumets)
  | ("DI"::lst,argumets) ->
     r0 := !r0 / !r1;
     simualte(lst,argumets)
  | ("MU"::lst,argumets) ->
     r0 := !r0 * !r1;
     simualte(lst,argumets)
  | ("AD"::lst,argumets) ->
     r0 := !r0 + !r1;
     simualte(lst,argumets)
  | ("PU"::lst,argumets) ->
     Stack.push !r0 stack;
     simualte(lst,argumets)
  | ("PO"::lst,argumets) ->
     r0 := (Stack.pop stack);
     simualte(lst,argumets)
  | ("SW"::lst,argumets) ->
     let tmp = !r0 in
     r0 := !r1;
     r1 := tmp;
     simualte(lst,argumets)
  | (op::lst,argumets) ->
     let op_code = String.sub op 0 2 in
     let value =
       int_of_string
         (String.sub op 3 ((String.length op) - 3))
     in
     match op_code with
     | "IM" ->
        r0 := value;
        simualte(lst,argumets)
     | "AR" ->
        r0 := List.nth argumets value;
        simualte(lst,argumets)
     | _ -> raise (CompilerError "bad assembly")

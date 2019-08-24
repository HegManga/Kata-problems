(*
You are writing a three-pass compiler for a simple programming language into a small assembly language.

The programming language has this syntax:

    function   ::= '[' arg-list ']' expression

    arg-list   ::= /* nothing */
                 | variable arg-list

    expression ::= term
                 | expression '+' term
                 | expression '-' term

    term       ::= factor
                 | term '*' factor
                 | term '/' factor

    factor     ::= number
                 | variable
                 | '(' expression ')'

Variables are strings of alphabetic characters. Numbers are strings of decimal digits representing integers. So, for example, a function which computes a2 + b2 might look like:

    [ a b ] a*a + b*b

A function which computes the average of two numbers might look like:

    [ first second ] (first + second) / 2

You need write a three-pass compiler. All test cases will be valid programs, so you needn't concentrate on error-handling.

The first pass will be the method pass1 which takes a string representing a function in the original programming language and will return a (JSON) object that represents that Abstract Syntax Tree. The Abstract Syntax Tree must use the following representations:

type ast =
  | Imm of int  (* immediate value *)
  | Arg of int  (* reference to n-th argument *)
  | Add of (ast * ast) (* add first to second *)
  | Sub of (ast * ast) (* subtract second from first *)
  | Mul of (ast * ast) (* multiply first by second *)
  | Div of (ast * ast) (* divide first by second *)

Note: arguments are indexed from zero. So, for example, the function

[ x y ] ( x + y ) / 2 would look like:

Div(Add(Arg 0,Arg 1), Imm 2)

The second pass of the compiler will be called pass2. This pass will take the output from pass1 and return a new Abstract Syntax Tree (with the same format) with all constant expressions reduced as much as possible. So, if for example, the function is [ x ] x + 2*5, the result of pass1 would be:

Add(Arg 0, Mul(Imm 2, Imm 5))

This would be passed into pass2 which would return:

Add(Arg 0, Imm 10)

The third pass of the compiler is pass3. The pass3 method takes in an Abstract Syntax Tree and returns an array of strings. Each string is an assembly directive. You are working on a small processor with two registers (R0 and R1), a stack, and an array of input arguments. The result of a function is expected to be in R0. The processor supports the following instructions:

    "IM n"     // load the constant value n into R0
    "AR n"     // load the n-th input argument into R0
    "SW"       // swap R0 and R1
    "PU"       // push R0 onto the stack
    "PO"       // pop the top value off of the stack into R0
    "AD"       // add R1 to R0 and put the result in R0
    "SU"       // subtract R1 from R0 and put the result in R0
    "MU"       // multiply R0 by R1 and put the result in R0
    "DI"       // divide R0 by R1 and put the result in R0

So, one possible return value from pass3 given the Abstract Syntax Tree shown above from pass2 is:

    [ "IM 10", "SW", "AR 0", "AD" ]

Here is a simulator for the target machine. It takes an array of assembly instructions and an array of arguments and returns the result.

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

let is_Arg argl str = List.mem str argl


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

let rec aux_AddSub expl expr lv l=
	match expr with
		|[] -> l@[expl]
		|h::t when h = "(" -> aux_AddSub (expl@[h]) t (lv+1) l
		|h::t when h = ")" -> aux_AddSub (expl@[h]) t (lv-1) l
		|h::t when (h = "+" || h = "-") && lv = 0 -> aux_AddSub [] t lv (l@[expl]@[[h]])
		|h::t ->  aux_AddSub (expl@[h]) t lv l
	;;

let rec aux_MulDiv expl expr lv l=
	match expr with
		|[] -> l@[expl]
		|h::t when h = "(" -> aux_MulDiv (expl@[h]) t (lv+1) l
		|h::t when h = ")" -> aux_MulDiv (expl@[h]) t (lv-1) l
		|h::t when (h = "*" || h = "/") && lv = 0 -> aux_MulDiv [] t lv (l@[expl]@[[h]])
		|h::t ->  aux_MulDiv (expl@[h]) t lv l
	;;

let rec aux_ast argl exp =
	let addsub = List.rev (aux_AddSub [] exp 0 []) in
	let rec aux_ast1 argl l =
		match l with
			|[] -> raise (Imposs("Not possible aux_ast1? []"))
			|[[x]] when is_Imm x -> Imm((int_of_string x))
			|[[x]] when is_Arg argl x -> Arg((listnth argl x))
			|[e] -> 
				let muldiv = List.rev (aux_MulDiv [] e 0 []) in
					let rec aux_ast2 argl lm=
						match lm with 
							|[] -> raise (Imposs("Not possible aux_ast1? []"))
							|[[x]] when is_Imm x -> Imm((int_of_string x))
							|[[x]] when is_Arg argl x -> Arg((listnth argl x))
							|f1::op::f2::t when op = ["*"] -> Mul(aux_ast2 argl (f2::t), aux_ast argl f1)
							|f1::op::f2::t when op = ["/"] -> Div(aux_ast2 argl (f2::t), aux_ast argl f1)
							|[e] -> 
								if (List.hd e) = "(" then aux_ast argl (List.tl (List.rev (List.tl (List.rev e))))
								else raise (Imposs("Not possible aux_ast2? e"))
							|_ -> raise (Imposs("Not possible aux_ast2? _"))
					in aux_ast2 argl muldiv
			|f1::op::f2::t when op = ["+"] -> Add(aux_ast1 argl (f2::t), aux_ast argl f1)
			|f1::op::f2::t when op = ["-"] -> Sub(aux_ast1 argl (f2::t), aux_ast argl f1)
			|_ -> raise (Imposs("Not possible aux_ast1? _"))
	in aux_ast1 argl addsub;;

let pass1 code =
	let (argl,exp) = split_codelist (tokenize code) 
	in
		aux_ast argl exp


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

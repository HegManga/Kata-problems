(*The goal of this kata is to embed a stack based language into OCaml. Languages such as forth have a number of simple commands such as push but are written in a postfix style. OCaml only has support for prefix and infix function application, can we directly embed such langauges into OCaml?

We will only use 4 instructions.

    begin_ - Marks the start of a program. (begin is reserved in OCaml)
    end_ - Marks the end of a program and returns the top element of the stack. (end is reserved in OCaml)

    push n - Pushes an integer onto he stack.
    add - Adds together the top two elements on the stack.

Here are some examples of programs written using these commands.

f = begin_ push 2 push 3 add end_
  = 5

g = begin_ push 1 push 1 add push 2 add end_
  = 4

There are several possible implementations. The two suggestions below are not required in order to complete this kata but further challenges if you so desire.

    An invalid program should raise a type error rather than a runtime error.
    Your implementation should allow programs such as..

     bad = begin_ push 1 push 1 push 1 push 1 push 1 push 1 push 1 push 1 push 1
             add add add add add add add add end_
*)

exception Exception of string;;

let begin_ f = f [] ;;

let push xs n f = f (n::xs);;

let add l f =
    match l with
        |(x :: y :: xs) -> f ((x + y)::xs)
        | _ -> raise  (Exception ("add"))
;;        
        
let end_ (l:'a list) =     
    match l with 
        |x::t -> x
        | _ -> raise (Exception ("Invalid Program"))
;;   

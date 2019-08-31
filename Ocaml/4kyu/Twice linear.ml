(*
Consider a sequence u where u is defined as follows:

    The number u(0) = 1 is the first one in u.
    For each x in u, then y = 2 * x + 1 and z = 3 * x + 1 must be in u too.
    There are no other numbers in u.

Ex: u = [1, 3, 4, 7, 9, 10, 13, 15, 19, 21, 22, 27, ...]

1 gives 3 and 4, then 3 gives 7 and 10, 4 gives 9 and 13, then 7 gives 15 and 22 and so on...
Task:

Given parameter n the function dbl_linear (or dblLinear...) returns the element u(n) of the ordered (with <) sequence u (so, there are no duplicates).
Example:

dbl_linear(10) should return 22
Note:

Focus attention on efficiency

*)

exception Imposs of string
let rec aux ((dl,tl) as l) n =
  if n <= 0 then 
    if List.hd dl < List.hd tl then List.hd dl else List.hd tl
  else
    match l with
      |([],[])-> raise (Imposs("aux"))
      |([],bh::bt) -> aux (([2*bh+1],bt@[3*bh+1])) (n-1)
      |(ah::at,[]) -> aux ((at@[2*ah+1],[3*ah+1])) (n-1)   
      |(ah::at,bh::bt) -> 
          if ah = bh then aux ((at@[2*ah+1],bt@[3*ah+1])) (n-1)
          else
            if ah < bh then aux ((at@[2*ah+1],(bh::bt)@[3*ah+1])) (n-1)
            else aux (((ah::at)@[2*bh+1],bt@[3*bh+1])) (n-1)
            
let dblLinear n = aux (([1],[])) n;; 

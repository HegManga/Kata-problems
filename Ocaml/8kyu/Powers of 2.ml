(*
Complete the function that takes a non-negative integer n as input, 
and returns a list of all the powers of 2 with the exponent ranging from 0 to n ( inclusive ).
Examples

n = 0  ==> [1]        # [2^0]
n = 1  ==> [1, 2]     # [2^0, 2^1]
n = 2  ==> [1, 2, 4]  # [2^0, 2^1, 2^2]
*)

let rec aux x n = 
  if n < 0 then x
  else  
    match x with
    |[] -> aux [1] (n-1)
    |h::t -> 
        let k = h*2
        in           
        aux (k::x) (n-1)
 
let rec reverse acc l =
  match l with
  |[] -> acc
  |h::t -> reverse (h::acc) t
    
let powersOfTwo n = reverse [] (aux [] n);;

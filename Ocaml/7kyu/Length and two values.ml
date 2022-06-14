(*
Given a number n and two values firstValue and secondValue, 
build an array of size n filled with firstValue and secondValue alternating.

for input:                5, true, false
expected result would be: [true, false, true, false, true]

Good luck!
*)

let alternate (n: int) (first_value: 'a) (second_value: 'a): 'a list =  
   let rec aux (m:int) (l: 'a list)= 
      match m with  
        |_ when m = 0 -> l
        |_ when (m mod 2 = 1) -> aux (m-1) (first_value::l)
        |_ -> aux (m-1) (second_value::l)
   in 
      aux n [];;

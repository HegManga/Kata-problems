(*

    A friend of mine takes a sequence of numbers from 1 to n (where n > 0).
    Within that sequence, he chooses two numbers, a and b.
    He says that the product of a and b should be equal to the sum of all numbers in the sequence, excluding a and b.
    Given a number n, could you tell me the numbers he excluded from the sequence?

The function takes the parameter: n (n is always strictly greater than 0) and returns an array or a string (depending on the language) of the form:

[(a, b), ...] or [[a, b], ...] or {{a, b}, ...} or or [{a, b}, ...]

with all (a, b) which are the possible removed numbers in the sequence 1 to n.

[(a, b), ...] or [[a, b], ...] or {{a, b}, ...} or ...will be sorted in increasing order of the "a".

It happens that there are several possible (a, b). The function returns an empty array (or an empty string) if no possible numbers are found which will prove that my friend has not told the truth! (Go: in this case return nil).

(See examples of returns for each language in "RUN SAMPLE TESTS")
Examples:

removNb(26) should return [(15, 21), (21, 15)]

or

removNb(26) should return { {15, 21}, {21, 15} }

or

removeNb(26) should return [[15, 21], [21, 15]]

or

removNb(26) should return [ {15, 21}, {21, 15} ]

or

removNb(26) should return "15 21, 21 15"

or

in C:
removNb(26) should return  **an array of pairs {{15, 21}{21, 15}}**
tested by way of strings.
*)

exception InvArg of string;;

let is_int v =  (v = (snd (modf v)))
let sumton n = if n>0 then n*(n+1)/2 else raise(InvArg("Err in sumton"))
let remov_aux n val_ l x = 
    let z = float_of_int x
    in
        let b = ((val_ -. z)/.(z+.1.0)) 
        in
            if is_int b && (int_of_float b)<n then (x,(int_of_float b))::l
            else l

let rec ltostring l =
    match l with
        |[] -> ""
        |[(x,y)] ->  Printf.sprintf "(%d, %d)" x y
        |((x,y))::t -> Printf.sprintf "(%d, %d)%s" x y (ltostring t)

let rec myiter f n i res =
    if i>(n/2) then myiter f n (i-1) (f res i)
    else res
        
let remov_nb (n: int): string =
    let val_ = sumton n   
    in 
        let s = myiter (remov_aux n (float_of_int val_)) n n [] 
        in
            ltostring s
  ;;

(*
If we list all the natural numbers below 10 that are multiples of 3 or 5, 
we get 3, 5, 6 and 9. The sum of these multiples is 23.

Finish the solution so that it returns the sum of all the multiples of 3 or 5 below the number passed in. 
Additionally, if the number is negative, return 0 (for languages that do have them).

Note: If the number is a multiple of both 3 and 5, only count it once.

Courtesy of projecteuler.net (Problem 1)

*)

let solve n =
    let m = n-1
    let i = (m-m%3)/3
    let j = (m-m%5)/5
    let k = (m-m%15)/15
    let n3 = 3*i*(i+1)
    let n5 = 5*j*(j+1)
    let n15 = 15*k*(k+1)
    (n3+n5-n15)/2
;;

(*
The aim of the kata is to try to show how difficult it can be to calculate 
decimals of an irrational number with a certain precision. 
We have chosen to get a few decimals of the number "pi" using the following infinite series (Leibniz 1646–1716):

PI / 4 = 1 - 1/3 + 1/5 - 1/7 + ... which gives an approximation of PI / 4.

http://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80

To have a measure of the difficulty we will count how many iterations 
are needed to calculate PI with a given precision of epsilon.

There are several ways to determine the precision of the calculus but to keep
things easy we will calculate PI within epsilon of your language Math::PI constant.

In other words, given as input a precision of epsilon we will stop the iterative 
process when the absolute value of the difference between our calculation using 
the Leibniz series and the Math::PI constant of your language is less than epsilon.

Your function returns an array or a string or a tuple depending on the language (See sample tests) with

    your number of iterations
    your approximation of PI with 10 decimals

OCaml : You can use "rnd10" (see "Your solution") to return PI with 10 decimals
Example :

Given epsilon = 0.001 your function gets an approximation of 3.140592653839794 
for PI at the end of 1000 iterations : since you are within epsilon of Math::PI you return

iter_pi(0.001) --> [1000, 3.1405926538]

Notes :

Unfortunately, this series converges too slowly to be useful, as it takes over 300 terms 
to obtain a 2 decimal place precision. To obtain 100 decimal places of PI, 
it was calculated that one would need to use at least 10^50 terms of this expansion!

About PI : http://www.geom.uiuc.edu/~huberty/math5337/groupe/expresspi.html
*)


let pi = 4.0 *. (atan 1.0)
let rnd10 x = 
  (x *. 1E10 |> int_of_float |> float_of_int) /. 1E10

let iter_pi (epsilon:float) =  
  let rec aux1 (n:int) (pvalue:float) =
    let diff = Float.abs (Float.sub pvalue  pi) in
    if diff < epsilon then (n, rnd10 pvalue)
    else aux2 (n+1) (Float.add pvalue (Float.div 4.0 (Float.add (Float.mul 2.0 (Float.of_int n)) 1.0)))
  and
    aux2 n pvalue =
    let diff = Float.abs (Float.sub pvalue  pi) in
    if diff < epsilon then (n,rnd10 pvalue)
    else aux1 (n+1) (Float.sub pvalue (Float.div 4.0 (Float.add (Float.mul 2.0 (Float.of_int n)) 1.0)))
  in 
  aux1 0 0.0
;;

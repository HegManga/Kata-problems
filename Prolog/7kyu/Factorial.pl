/*
Yor task is to write function factorial
https://en.wikipedia.org/wiki/Factorial
*/

factorial(0,1).
factorial(N,R) :-
  N1 is N - 1,
  factorial(N1,Rt),
  R is Rt*N.

/*
Given two integers a and b, which can be positive or negative, find the sum of all the numbers between including them too and return it. If the two numbers are equal return a or b.
Note: a and b are not ordered!
Examples

get_sum(1, 0, 1) % 1 + 0 = 1
get_sum(1, 2, 3) % 1 + 2 = 3
get_sum(0, 1, 3) % 0 + 1 = 1
get_sum(1, 1, 1)  % 1 Since both are same
get_sum(-1, 0, -1) % -1 + 0 = -1
get_sum(-1, 2, 2) % -1 + 0 + 1 + 2 = 2
*/
get_sum(A, B, Result) :-
  A=:=B, Result is A;
  A is 0, B>0, Result is B*(B+1)/2;
  A is 0, B<0, Result is B*(-B+1)/2;
  B is 0, A>0, Result is A*(A+1)/2;
  B is 0, A<0, Result is A*(-A+1)/2;
  A>B, B>0, Result is A*(A+1)/2 - B*(B-1)/2;
  B>A, B>0, Result is B*(B+1)/2 - A*(A-1)/2;
  A<0, B<A, Result is B*(-B+1)/2 - A*(-A-1)/2;
  A<B, B<0, Result is A*(-A+1)/2 - B*(-B-1)/2;
  A>0, B<0, Result is A*(A+1)/2 + B*(-B+1)/2;
  A<0, B>0, Result is B*(B+1)/2 + A*(-A+1)/2.
  
  
  /*
  Better solution proposed by "Unnamed"
  get_sum(A, B, Result) :-
  Result is (A + B) * (abs(A - B) + 1) div 2.
  */

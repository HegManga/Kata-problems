/*
-Definition
Extra perfect number is the number that first and last bits are set bits.

-Task
Given a positive integer N , Return the extra perfect numbers in range from 1 to N .
Warm-up (Highly recommended)
Playing With Numbers Series
Notes
    Number passed is always Positive .
    Returned array/list should contain the extra perfect numbers in ascending order from lowest to highest

-Input >> Output Examples

extraPerfect(3)  ==>  return {1,3}
Explanation:
(1)10 =(1)2
First and last bits as set bits.
(3)10 = (11)2
First and last bits as set bits.

extraPerfect(7)  ==>  return {1,3,5,7}
Explanation:
(5)10 = (101)2
First and last bits as set bits.
(7)10 = (111)2
First and last bits as set bits.
*/

extra_perfect_odd(Number,Number,[Number]).
extra_perfect_odd(Number,I,[I|Temp]) :-
  I1 is I+1, extra_perfect_even(Number,I1,Temp).

extra_perfect_even(Number,Number,[]).
extra_perfect_even(Number,I,Result) :-
  I1 is I+1, extra_perfect_odd(Number,I1,Result).
  
extra_perfect(Number, Result) :-
  extra_perfect_odd(Number,1,Result).

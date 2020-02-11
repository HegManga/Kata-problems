/*
Given u0 = 1, u1 = 2 and the relation 6unun+1-5unun+2+un+1un+2 = 0 calculate un for any integer n >= 0.

#Examples
fcn(n) returns un: fcn(17) -> 131072, fcn(21) -> 2097152

Remark: You can take two points of view to do this kata:
    the first one purely algorithmic from the definition of un
    the second one - not at all mandatory, but as a complement - is to get a bit your head around and find which sequence is hidden behind un.
*/
%1st version
fcn_aux(N,I,U0,U1,R) :-
	I is N+1, R is U1;
	I1 is I+1,U2 is 6*U1*U0/(5*U0-U1), fcn_aux(N,I1,U1,U2,R).
	
fcn(0,1).
fcn(1,2).
fcn(N, R) :- fcn_aux(N,2,1,2,R).

%2nd version
fcn2(N, R) :- R is 2^N.

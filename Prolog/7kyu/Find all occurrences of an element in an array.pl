/*
Given an array (a list in Python) of integers and an integer n, find all occurrences of n in the given array and return another array containing all the index positions of n in the given array.
If n is not in the given array, return an empty array [].
Assume that n and all values in the given array will always be integers.

Example:
find_all([6, 9, 3, 4, 3, 82, 11], 3) = [2, 4]
*/

% Store the result inside 3rd argument. Good luck!
find_all_aux([N],N,[I],I).
find_all_aux([],_,[],_).
find_all_aux([N|Tail], N, [I|Result],I) :-
		I1 is I+1,find_all_aux(Tail,N,Result,I1).
find_all_aux([_|Tail], N, Result,I) :-
        I1 is I+1,find_all_aux(Tail,N,Result,I1).
              
find_all(List, N, Result) :- 
        find_all_aux(List,N,Result,0).

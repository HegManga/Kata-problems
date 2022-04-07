(*
Write a function that checks if a given string (case insensitive) is a palindrome.
*)
let isPalindrom s =
   let l = String.length s
   in
     let rec aux (str : string) i n =
        match (i,n) with
        |(n,-1) -> true
        |(i,j) when str.Chars(i) = str.Chars(j) -> aux str (i+1) (j-1) 
        |(i,j) when (int str.[i]) - 65 + 97 = int (str.[j]) -> aux str (i+1) (j-1) 
        |(i,j) when (int str.[j]) - 65 + 97 = int (str.[i]) -> aux str (i+1) (j-1) 
        |_ -> false
     in 
        aux s 0 (l-1)   
     ;;

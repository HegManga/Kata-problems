(*
Given u0 = 1, u1 = 2 and the relation 6unun+1-5unun+2+un+1un+2 = 0 calculate un for any integer n >= 0.
Examples:

Call fcn the function such as fcn(n) = un.

fcn(17) -> 131072; fcn(21) -> 2097152
Remark:

You can take two points of view to do this kata:

    the first one purely algorithmic from the definition of un

    the second one - not at all mandatory, but as a complement - 
    is to get a bit your head around and find which sequence is hidden behind un.
*)

(*
Clever, not mine
open Big_int;;

let fcn(n: int): string =
  (string_of_big_int (power_big_int_positive_int (big_int_of_int 2) n)) ;;
*)
let string_rev (str:string) =
  let rec aux  idx = match idx with
      0 -> Char.escaped (str.[0])
    | _ -> (Char.escaped str.[idx]) ^ (aux (idx-1)) 
  and l = String.length str in
  if l>0 then aux (l-1) else ""  ;;

let string_pop (str:string) =
  let l = (String.length str)-1
  in String.sub str 1 l;;

let string_first (str:string) =
  String.sub str 0 1;;

let add_dig (a:string) (b:string) = 
  let rec aux_ad c d=
    if  d="0" then c
    else if c="" then d
    else 
      let e = int_of_string (string_first c) and f = int_of_string d in 
      string_of_int((e+f) mod 10)^(aux_ad (string_pop c) (string_of_int((e+f)/10)))
  in string_rev (aux_ad (string_rev a) b);;    

let add (a:string) (b:string) =
  let rec aux c d =
    match (c,d) with 
    |("",d) when d="0" -> ""
    |("",d) -> d
    |(c,"") when c="0" -> ""
    |(c,"")-> c  
    |(c,d) -> let e = int_of_string (string_first c) and f =int_of_string (string_first d) in 
        string_of_int((e+f) mod 10) ^ (aux (aux (string_pop c) (string_pop d)) (string_of_int((e+f)/10)))
  in
  string_rev (aux (string_rev a) (string_rev b));;

let mul_dig (a:string) (b:string) = 
  let rec aux_md c d=
    if  d="0" then "0"
    else if d ="1" then c
    else if c="" then ""
    else 
      let e = int_of_string (string_first c) and f = int_of_string d in 
      string_of_int((e*f) mod 10)^(string_rev(add_dig (string_rev (aux_md (string_pop c) d)) (string_of_int((e*f)/10))))
  in string_rev (aux_md (string_rev a) b);;   

let mul (a:string) (b:string) =   
  let rec aux c d =
    match (c,d) with 
    |(_,d) when d="0" -> "0"
    |(_,"") -> ""
    |(c,_) when c="0" -> "0"
    |(c,"1")-> c  
    |(c,d) ->
        add (mul_dig c (string_first d)) ((aux c (string_pop d)^"0"))
  in
  aux a (string_rev b);;

let rec pow a= function
  | 0 -> "1"
  | 1 -> a
  | k -> 
      let b = pow a (k/2) in
      mul b  (mul b  (if  ( k mod 2 )= 0 then "1" else a)) ;;

let fcn(n:int): string = pow "2" n ;

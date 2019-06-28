(*Given two strings s1 and s2, we want to visualize how different the two strings are. We will only take into account the lowercase letters (a to z). First let us count the frequency of each lowercase letters in s1 and s2.

s1 = "A aaaa bb c"

s2 = "& aaa bbb c d"

s1 has 4 'a', 2 'b', 1 'c'

s2 has 3 'a', 3 'b', 1 'c', 1 'd'

So the maximum for 'a' in s1 and s2 is 4 from s1; the maximum for 'b' is 3 from s2. In the following we will not consider letters when the maximum of their occurrences is less than or equal to 1.

We can resume the differences between s1 and s2 in the following string: "1:aaaa/2:bbb" where 1 in 1:aaaa stands for string s1 and aaaa because the maximum for a is 4. In the same manner 2:bbb stands for string s2 and bbb because the maximum for b is 3.

The task is to produce a string in which each lowercase letters of s1 or s2 appears as many times as its maximum if this maximum is strictly greater than 1; these letters will be prefixed by the number of the string where they appear with their maximum value and :. If the maximum is in s1 as well as in s2 the prefix is =:.

In the result, substrings (a substring is for example 2:nnnnn or 1:hhh; it contains the prefix) will be in decreasing order of their length and when they have the same length sorted in ascending lexicographic order (letters and digits - more precisely sorted by codepoint); the different groups will be separated by '/'. See examples and "Example Tests".

Hopefully other examples can make this clearer.

s1 = "my&friend&Paul has heavy hats! &"
s2 = "my friend John has many many friends &"
mix(s1, s2) --> "2:nnnnn/1:aaaa/1:hhh/2:mmm/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

s1 = "mmmmm m nnnnn y&friend&Paul has heavy hats! &"
s2 = "my frie n d Joh n has ma n y ma n y frie n ds n&"
mix(s1, s2) --> "1:mmmmmm/=:nnnnnn/1:aaaa/1:hhh/2:yyy/2:dd/2:ff/2:ii/2:rr/=:ee/=:ss"

s1="Are the kids at home? aaaaa fffff"
s2="Yes they are here! aaaaa fffff"
mix(s1, s2) --> "=:aaaaaa/2:eeeee/=:fffff/1:tt/2:rr/=:hh"
*)
open Printf;;
type ordlist = OList of (int list);;

let ordlist_empty = OList([]);; 
let ordlist_isEmpty (OList(l)) = if l = [] then true else false;;
let ordlist_get (OList(l))= l;;
let ordlist_add (OList(l)) c =
    let i = (Char.code c) and a =( Char.code 'a') and z =( Char.code 'z') 
    in 
        if (i>=a && i<=z) then
            (let rec aux la x =( 
                match la with
                    |[] -> [x]
                    |h::t when h = x -> la
                    |h::t when h > x -> x::h::t
                    |h::t  -> h::(aux t x)
                    )
            in  
                OList(aux l i)
            )        
        else OList(l)   
;;

let ordlist_remove (OList(l)) c =
    let i = (Char.code c) and a =( Char.code 'a') and z =( Char.code 'z') 
    in 
        if (i>=a && i<=z) then
            (let rec aux la x =( 
                match la with
                    |[] -> [x]
                    |h::t when h = x -> t
                    |h::t when h > x -> la
                    |h::t  -> h::(aux t x)
                    )
            in  
                OList(aux l i)
            )        
        else OList(l)   
;;
 
type encodetag = Trash| One| Many of int;;
type encode = char -> encodetag;;

let encode_empty = fun (x:char) -> Trash;;
let encode_increment enc c = 
    let i = (Char.code c) and a =( Char.code 'a') and z =( Char.code 'z') 
    in  (
        if (i>=a && i<=z) then
            fun x ->
                if x = c then
                (match (enc c) with
                    |Trash -> One
                    |One -> Many(2)
                    |Many(x) -> Many(x+1)
                )  
                else enc x
        else enc 
    )    
;;   
let encode_get enc c =
    match (enc c) with
        |Trash -> 0
        |One -> 1
        |Many(x) -> x
;;

type f_ordlist = Fol of (int -> ordlist);;
    
let fol_empty = let e = ordlist_empty in Fol(fun (x:int) -> e);;
let fol_isEmpty (Fol(fol)) i = ordlist_isEmpty (fol i);; 
let fol_add (Fol(fol)) i c = ordlist_add (fol i) c;;
let fol_remove (Fol(fol)) i c = ordlist_remove (fol i) c;;    

let fol_increment (Fol(fol)) enc c =
    match (enc c) with
        |Trash -> 
            (let a = (fol_add (Fol(fol)) 1 c) 
            in 
                (Fol(fun x -> if x =1 then a else fol x)))
        |One ->
            (let a = (fol_add (Fol(fol)) 2 c) and r = (fol_remove (Fol(fol)) 1 c)
            in 
                (Fol(function 
                    | 1 -> r
                    | 2 -> a
                    | x -> fol x
                    )
                )
             )   
        |Many(x) -> 
            (let a = (fol_add (Fol(fol)) (x+1) c) and r = (fol_remove (Fol(fol)) x c)
            in 
                (Fol(function 
                    | z when z = x -> r
                    | z when z = (x+1) -> a
                    | z -> fol z
                    )
                )
             )   
;;    

let rec list_add l n = 
    match l with
        |[] -> [n]
        |h::t when h = n -> l
        |h::t when h < n -> n::l
        |h::t -> h::(list_add t n)
;;   

let rec list_remove l n = 
    match l with
        |[] -> [n]
        |h::t when h = n -> t
        |h::t when h < n -> l
        |h::t -> h::(list_remove t n)
;;         

type store = Store of (int list) * f_ordlist * encode;;   
 
let store_empty = Store([],fol_empty, encode_empty)
;;
let store_l (Store(l,fol,enc)) i = 
    if (fol_isEmpty fol i) = true then list_add (list_remove l i) (i+1)
    else list_add l (i+1)
;;    

let store_increment (Store(l,fol,enc)) c = 
    let il = encode_get enc c and nfol = (fol_increment fol enc c) 
    and nenc = (encode_increment enc c)
    in
        Store((store_l (Store(l,nfol,nenc)) il),nfol,nenc)
;;    
let rec myiter f (ss1: string) =
    match ss1 with
        |"" -> store_empty;
        |s -> store_increment (myiter f (String.sub ss1 1 ((String.length ss1)-1))) (String.get ss1 0)    
;;    
let store_fromstring (ss1 : string) = 
    myiter (store_increment (store_empty)) ss1
;;

type slist = Slist of int * int * char;;

let fol_toslist (Fol(fol)) i = 
    let rec aux l =
        (match l with
            |[] -> []
            |h::t -> (Slist(0,i,Char.chr h))::(aux t)
        )
    in aux (ordlist_get (fol i))
;;    
let rec store_toslist ((Store(l,fol,enc)) as _store)=
    match l with
        |[] -> []
        |[x] when x=1 -> []
        |h::t -> (fol_toslist fol h)@(store_toslist (Store(t,fol,enc)))
;;         

let rec slist_remove sl c=
    match sl with 
        |[] -> []
        |(Slist(_,_,x))::t when x=c -> t
        |h::t -> h::(slist_remove t c)
;;
let rec slist_best sl1 sl2 = 
    match (sl1,sl2) with
        |([],[]) -> []
        |([],(Slist(_,n2,c2))::t2)-> (Slist(2,n2,c2)):: (slist_best [] t2)
        |((Slist(_,n1,c1))::t1,[])-> (Slist(1,n1,c1)):: (slist_best t1 [])
        |((Slist(_,n1,c1))::t1,(Slist(_,n2,c2))::t2) when n1 > n2 -> (Slist(1,n1,c1))::(slist_best t1 (slist_remove sl2 c1))
        |((Slist(_,n1,c1))::t1,(Slist(_,n2,c2))::t2) when n1 < n2 -> (Slist(2,n2,c2))::(slist_best (slist_remove sl1 c2) t2)
        |((Slist(_,n1,c1))::t1,(Slist(_,n2,c2))::t2) when c1 < c2 -> (Slist(1,n1,c1))::(slist_best t1 (slist_remove sl2 c1))
        |((Slist(_,n1,c1))::t1,(Slist(_,n2,c2))::t2) when c1 > c2 -> (Slist(2,n2,c2))::(slist_best (slist_remove sl1 c2) t2)
        |((Slist(_,n1,c1))::t1,(Slist(_,n2,c2))::t2) -> (Slist(3,n1,c1))::(slist_best t1 t2)
 ;;   


let rec string_nchar c n = 
    if n > 0 then (sprintf "%c" c)^(string_nchar c (n-1) )
    else ""
;;

let rec slist_reorderaux (sl,boolean) =
    match sl with
        |[] -> ([],true);
        |x::[] -> (x::[],true);
        |(Slist(i,ni,x))::(Slist(j,nj,y))::t when nj>ni -> 
                let (sub_sl,sub_boolean) = ( slist_reorderaux ((Slist(i,ni,x))::t,boolean) )
                in ((Slist(j,nj,y))::sub_sl, false)
        |(Slist(i,ni,x))::(Slist(j,nj,y))::t when nj<ni || i<=j-> 
                let (sub_sl,sub_boolean) = (slist_reorderaux ((Slist(j,nj,y))::t,boolean) )
                in ((Slist(i,ni,x))::sub_sl, sub_boolean)
        |(Slist(i,ni,x))::(Slist(j,nj,y))::t -> 
                let (sub_sl,sub_boolean) = (slist_reorderaux ((Slist(i,ni,x))::t,boolean))
                in ((Slist(j,nj,y))::sub_sl, false)
;;    
let rec slist_size sl n=
    match sl with
        |[] -> n
        |h::t -> slist_size t (n+1)
;;
let slist_reorder sl =   
    let rec aux f (sl,b) n = (if n>0 && b=false then aux f (f (sl,b)) (n-1) else (sl,b)) and aux1 (sl,b) = sl
    in aux1 (aux (slist_reorderaux) (sl,false) (slist_size sl 0)) 
;;
let rec slist_tosol sl =
    match sl with
        |(Slist(1,n,x))::t when n>1 -> sprintf "/1:%s%s" (string_nchar x n) (slist_tosol t)
        |(Slist(2,n,x))::t when n>1 -> sprintf "/2:%s%s" (string_nchar x n) (slist_tosol t)
        |(Slist(3,n,x))::t when n>1 -> sprintf "/=:%s%s" (string_nchar x n) (slist_tosol t)
        |(Slist(0,n,x))::t when n>1 -> sprintf "/0:%s%s" (string_nchar x n) (slist_tosol t) (*DEBUG*)
        |x::t -> sprintf "%s" (slist_tosol t)
        |_ -> ""
;;        
     
let mix (ss1: string) (ss2: string): string = 
    let s1 = store_fromstring ss1 and s2 = store_fromstring ss2
    in  
        let sl1 = store_toslist s1 and  sl2 = store_toslist s2 
        in
            let sl = slist_reorder (slist_best sl1 sl2)
                in let s =slist_tosol sl
                    in let d = (String.length s)-1 
                        in if d > 1 then String.sub s 1 d else s
;;

    

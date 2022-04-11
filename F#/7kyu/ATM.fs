(*
An ATM has banknotes of nominal values 10, 20, 50, 100, 200 and 500 dollars. 
You can consider that there is a large enough supply of each of these banknotes.

You have to write the ATM's function that determines the minimal number of banknotes 
needed to honor a withdrawal of n dollars, with 1 <= n <= 1500.

Return that number, or -1 if it is impossible.
*)

let solve n =
    let bn = [500;200;100;50;20;10]
    in 
      let rec aux (n:int) (b:int list) (res:int)= 
        match(n,b) with
          |(0,_) -> res
          |(n,h::t) when n>=1 && n <=1500 -> aux (n%h) t (res+n/h)
          |_-> -1
      in
        aux n bn 0
;;

let powersOfTwo n =
  let rec aux acc = function
    | 0 -> []
    | n -> acc * 2 :: aux (acc * 2) (n - 1)
  in
  1 :: aux 1 n

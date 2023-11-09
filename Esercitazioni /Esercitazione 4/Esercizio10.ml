let consecutive_even l =
  let rec count n l = match l with
  (n, []) -> 0
  |(n, l1::l2::l3) when ((l1 mod 2) = 0) && ((l2 mod 2) = 0) -> count n+1 (l2::l3)
  |(n, l1::l2::l3) when ((l1 mod 2) != 0) || ((l2 mod 2) != 0) -> count 0 (l2::l3)
in count 0 l
;;

assert(consecutive_even [] = 0);;

assert(consecutive_even [1;2;3;4;5;6] = 1);; 

assert(consecutive_even [1;2;2;3;4;5] = 2);;
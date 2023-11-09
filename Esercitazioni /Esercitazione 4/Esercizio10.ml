let consecutive_even l =
  let rec count n max l'= match (n, l') with
  (n, []) -> max
  |(n, l1::l2) when (l1 mod 2) = 0 -> if(max > n) then count (n+1) max l2 else count (n+1) (n+1) l2
  |(n, l1::l2) when (l1 mod 2) <> 0 -> count 0 max l2
in count 0 0 l
;;

assert(consecutive_even [] = 0);;

assert(consecutive_even [1;2;3;4;5;6] = 1);; 

assert(consecutive_even [1;2;2;3;4;5] = 2);;
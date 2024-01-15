(*Esercizio 1: Absolute Value*)

let f x = if x > 0 then x else -x;;

(*Esercizio 2: Max of two natural numbers*)

let f x y = if x >= y then x else y;;

(*Esercizio 3: NAND with pattern matching*)

let nand a b = match (a,b) with
(false, false) -> true
|(false, true) -> true
|(true, false) -> true
|(true, true) -> false
;;

(*Esercizio 4: Parrot Trouble*)



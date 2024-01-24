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

(*Esercizio 4: Weekly lectures*)
type weekday = Mo | Tu | We | Th | Fr
type course = ALF | LIP

let isLecture c w = match (c,w) with
(ALF,w) when w == Tu || w == Th || w == Fr -> true
|(LIP,w) when w == We || w == Th -> true
|_ -> false
;;

(*Esercizio 5: Parrot Trouble*)
let parrot_trouble t h = match (t,h) with
(true,h) when h >= 0 && h <= 7 -> Some true
|(true,h) when h >= 20 && h <= 23 -> Some true
|(t,h) when h < 0 || h > 23 -> None
|_ -> Some false
;;

(*Esercizio 6: *)


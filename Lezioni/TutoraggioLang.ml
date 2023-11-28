(* Parole che non hanno occorrenze consecutive di 0 *)

type state = Q0 | Q1;;
type label = A0 | A1;;

let step1 q a = match (q,a) with
 (Q0, A0) -> Q1
|(Q0, A1) -> Q0
|(Q1, A1) -> Q0
| _ -> failwith "no transition"
;;

let rec step q w = match w with
[] -> q 
|a::w' -> step (step1  q a) w'
;;

(* List.mem prende in input un elemento e controlla se appartiene 
   alla lista che gli passo, in questo caso gli sto passando una lista di stati*)

let lang w = try List.mem(step Q0 w) [Q0;Q1] with _ -> false;;

(* Parole che contengono un numero di 0 maggiore del numero di 1 *)

let rec count a = function
[] -> 0
|b::w -> (if a = b then 1 else 0) + count a w;;

let lang3 w = count A0 w > count A1 w;;

let step1 (a,b) = function
A0 -> (a+1,b)
|A1 -> (a,b+1);;

let rec step q = function 
[] -> q 
|h::w -> step (step1 q h) w;;

let lang3 w = let(a,b) = step (0,0) w in a>b;; 
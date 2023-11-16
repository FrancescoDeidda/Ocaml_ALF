(* Esercizio 1 -> Reverse a list *)

let rec rev = function
([]) -> []
| (t::[]) -> [t]
| (t::h) -> rev h @ rev [t];;

rev [1;2;3;4];;

(* Esercizio 2 -> String of list *)

let rec concat = function 
([]) -> "[]"
| (t::[]) -> string_of_int(t) ^ "]"
| (t::h) -> string_of_int(t) ^ ";" ^ concat h;;

let string_of_list l = "[" ^ concat l;;

string_of_list [];;
string_of_list [1];;
string_of_list [1;2;3];;


(* Esercizio 3 -> Random List *)

let rnd_list n b = 
  let rec aux n b l = match (n, b, l) with
(n,b,l) when n = 0 -> l
| (n,b,l) -> aux (n-1) b ([Random.int(b)] @ l)
in aux n b [];;

rnd_list 3 6;;

(* Esercizio 4 -> Rotate List *)

let rec rotate n (l: 'a list) = match (n, l) with
 (n, []) when n > 0 -> []
| (n, l1::l2) when n> 0 -> rotate (n-1) (l2 @ [l1])
| (n, l) when n = 0 -> l;;

(* Esercizio 5 -> Consecutive Even *)

let consecutive_even l =
  let rec count n max l'= match (n, l') with
  (n, []) -> max
  |(n, l1::l2) when (l1 mod 2) = 0 -> if(max > n) then count (n+1) max l2 else count (n+1) (n+1) l2
  |(n, l1::l2) when (l1 mod 2) <> 0 -> count 0 max l2
in count 0 0 l
;;


(* Esercizio 6 -> Enumeration of integer *)

let enum_int n = if(n mod 2 = 0) then n/2 else (-((n/2)+1));;

(* Esercizio 7 -> Enumeration of pairs of naturals *)

let rec enum_nat_nat n = match n with
(0) -> (0,0)
| n -> match (enum_nat_nat (n-1)) with
(x,0) -> (0,x+1)
|(x,y) -> (x+1, y-1);;

(* Esercizio 8 -> Peano Artihmetics *)

type nat = Z | S of nat;;

let rec iseven n = match n with
Z -> true
|S x -> not(iseven x);;

let rec sub x y =  match (x,y) with
(S x, S y) -> sub x y
|(x, Z) -> x
|(Z,_) -> failwith "errore";;

let rec less a b = match (a,b) with
(Z, S a) -> true
|(S a, S b) -> less a b
|_ -> false
;;

let rec halve n = match n with
Z -> Z
|x when not(less x (S(S Z))) -> S (halve (sub x (S(S Z))))
| _ -> Z;;


let rec add x y = match (x,y) with
(Z, a) -> a
|(a, Z) -> a
|(S a, b) -> add a (S b);;


let rec mul x y = match (x,y) with
(Z,_) -> Z
|(_,Z) -> Z
|(S a, b) -> add b (mul a b);;


let rec equals a b = match (a,b) with
(Z,Z) -> true
|(S a, S b ) -> equals a b
| _ -> false;;


let rec leq a b = match (a,b) with
(Z,_) -> true
|(S a, S b) -> leq a b
|_ -> false;;

(* Esercizio 9 -> Bit String *)

type bitstring = E | Z of bitstring | U of bitstring;;

let rec string_of_bitstring (s : bitstring) : string = match s with
E -> ""
| Z s1 -> "0" ^ string_of_bitstring s1
| U s1 -> "1" ^ string_of_bitstring s1;;

let rec len (s: bitstring) = match s with
E -> 0
|Z s1 -> 1 + len s1
|U s1 -> 1 + len s1;;

let rec countZ (s:bitstring) = match s with
E -> 0
| Z s1 -> 1 + countZ s1
| U s1 -> countZ s1;;

let rec countU (s:bitstring) = match s with
E -> 0
| Z -> countU s1
| U -> 1 + countU s1;;

let rec concat (s1.bitstring) (s2:bitstring) = match s1 s2 with
E, s2 -> s2
| s1, E -> s1
| U s1, s2 -> U(concat s1 s2)
| Z s1, s2 -> Z(concat s1 s2)
;;




let foo x = 
  let n = (x+1) in 
  let m = n*n
  in m-1;;

foo 3;;

let pipe v f = f v;;

let foo2 x = 
  pipe (pipe (pipe x (fun x -> x+1)) (fun x -> x*x)) (fun x -> x-1) ;;

3 |> (fun x -> x+1);;

let foo3 x = x |> (fun x -> x+1) |> (fun x -> x*x) |> (fun x -> x-1);;

let rec last = function
    [] -> failwith "lista vuota"
  | [a] -> a
  | _::l -> last l
;;

let rec sum = function
    [] -> 0
  | a::l -> a + sum l;;

type t = I of int | B of bool;;

let rec sumt = function
    [] -> 0
  | a::l -> sumt l + (match a with 
        I n -> n
      | B b -> if b then 1 else 0);;
  
let rec sumt2 = function
    [] -> 0
  | (I n)::l -> n + sumt2 l 
  | (B true)::l -> 1 + sumt2 l 
  | _::l -> sumt2 l;;

let rec incl = function
    [] -> []
  | a::l -> (a+1)::(incl l);;

let rec sql = function
    [] -> []
  | a::l -> (a*a)::(sql l);;

let fool l = l |> incl |> sql |> sum;;

(* map : 'a -> 'b -> 'a list -> 'b list *)

let rec map f = function 
    [] -> []
  | a::l -> (f a)::(map f l);;

map (fun x -> x+1) [1;2;3];;

let foomap l = sum (map (fun x -> x*x) (map (fun x -> x+1) l ));;

let foomap2 l = 
  l 
  |> (map (fun x -> x+1))
  |> (map (fun x -> x*x))
  |> sum
;;

(* filter : ('a -> bool) -> 'a list -> 'a list *)

let rec filter f = function
    [] -> []
  | a::l -> if f a then a::(filter f l) else filter f l;;

filter (fun x -> x>=0) [1;-2;3];;
filter (fun x -> x mod 2 = 0) [1;-2;3;4];;

let foomap3 l = 
  l 
  |> (filter (fun x -> x>=0))
  |> (map (fun x -> x+1))
  |> (map (fun x -> x*x))
  |> sum
;;

foomap3 [1;-1;-2;2;3;-5];;

List.fold_left;;

let sum2 l = List.fold_left (fun x y -> x + y) 0 l;;

(* mem: 'a -> 'a list -> bool *)
let rec mem a = function
    [] -> false
  | b::l -> if b=a then true else mem a l;;

let mem2 a l = List.fold_left (fun b y -> b || y=a) false l;;

let rec count a = function
    [] -> 0 
  | b::l -> (if b=a then 1 else 0) + count a l;;

let count2 a l = List.fold_left (fun n y -> n + (if a=y then 1 else 0)) 0 l;;

(* string_of_int: int -> string *)
(* string concatenation operator: ^ *)

let string_of_intlist l = 
  "[" ^
  (List.fold_left (fun s n -> s ^ (if s="" then "" else ";") ^ (string_of_int n)) "" l)
  ^ "]";;
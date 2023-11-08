type aexp = 
 Const of int 
| Var of string 
| Add of aexp * aexp
| Sub of aexp * aexp
| Mul of aexp * aexp
;;

type bexp =
 True
| False
| Not of bexp
| And of bexp * bexp
| Leq of aexp * aexp;;

type cmd = 
 Skip
| Assign of string * aexp
| Seq of cmd * cmd
| If of bexp * cmd * cmd
| While of bexp * cmd
;;

let rec sem_aexp sigma = function
 Const n -> n
| Var x -> sigma x
| Add(e0,e1) -> sem_aexp sigma e0 + sem_aexp sigma e1
| Sub(e0,e1) -> sem_aexp sigma e0 - sem_aexp sigma e1
| Mul(e0,e1) -> sem_aexp sigma e0 * sem_aexp sigma e1 
;;

let rec sem_bexp sigma = function
    True -> true
  | False -> false
  | Not b -> not (sem_bexp sigma b)
  | And(b0,b1) -> (sem_bexp sigma b0) && (sem_bexp sigma b1)
  | Leq(e0,e1) -> (sem_aexp sigma e0) <= (sem_aexp sigma e1)
;;

let e0 = Add(Var "x", Const 1);;
let sigma0 = fun _ -> failwith "undefined";;

sem_aexp sigma0 e0;;

let sigma1 = fun z -> if z="x" then 7 else sigma0 z;; 
sem_aexp sigma1 e0;;

let bind f x n = fun y -> if y=x then n else f y;;

let rec sem_cmd sigma = function
    Skip -> sigma
  | Assign(x,e) -> bind sigma x (sem_aexp sigma e)
  | If(b,c0,c1) -> if sem_bexp sigma b 
      then sem_cmd sigma c0 
      else sem_cmd sigma c1
  | Seq(c0,c1) -> let sigma' = sem_cmd sigma c0 
      in sem_cmd sigma' c1
  | While(b,c) -> if not (sem_bexp sigma b) then sigma
      else sem_cmd sigma (Seq(c,While(b,c)))
;;
(* 
 * / 1
 * x / y  if y=x+6
 * x / y  if y=2x
 *)
 let rec chi = function 
 1 -> true
| y when y>0 -> (y>6 && chi(y-6)) || (y mod 2 = 0 && chi(y/2))
;;

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

(* skip; x=3; if true then x=x+1 else skip *)

Seq(Skip,Seq(Assign("x",Const 3),If(True,Assign("x",Add(Var "x", Const 1)),Skip)))
;;


let rec sem_aexp sigma = function
 Const n -> n
| Var x -> sigma x
| Add(e0,e1) -> sem_aexp sigma e0 + sem_aexp sigma e1
| Sub(e0,e1) -> sem_aexp sigma e0 - sem_aexp sigma e1
| Mul(e0,e1) -> sem_aexp sigma e0 * sem_aexp sigma e1 
;;

let e0 = Add(Var "x", Const 1);;
let sigma0 = fun _ -> failwith "undefined";;

sem_aexp sigma0 e0;;

let sigma1 = fun z -> if z="x" then 7 else sigma0 z;;

sem_aexp sigma1 e0;;


let s0 = [("x",1); ("y",2)];;
List.assoc "y" s0;;
(* Esercizio 4 -> Tris *)

let tris (a,b,c,d) : (bool) = match (a,b,c,d) with
    (a,b,c,_) when a=b && a=c -> true
  | (a,b,_,d) when a=b && a=d -> true
  | (a,_,c,d) when a=c && a=d -> true
  | (_,b,c,d) when c=b && b=d -> true
  | _ -> false;;

(* Esercizio 5 -> Poker *)

type suit = S | H | D | C;;
type card = Card of int * suit;;

let suitEx () = match (Random.int(4)) with
    0 -> (S, 1+Random.int(13))
  | 1 -> (H, 1+Random.int(13))
  | 2 -> (D, 1+Random.int(13))
  | 3 -> (C, 1+Random.int(13))

let rndHand () =
  (suitEx(),suitEx(),suitEx(),suitEx(),suitEx());;

rndHand();; 

(* Esercizio 7 -> Loaded Dice *)
let dice p = 
  if((1+Random.int(100))<= p) then 6 else 1+Random.int(5);;

dice 70;;


(* Esercizio 8 -> Morra *)

type winner = Player | Computer | Tie ;;

let win (hp,gp) = 
  let hc = Random.int(5) and gc = Random.int(10) in
  if ((hp+hc) = gc) then ((hc,gc),Computer)
  else if ((hp+hc) = gp) then ((hc,gc),Player)
  else ((hc,gc),Tie);;
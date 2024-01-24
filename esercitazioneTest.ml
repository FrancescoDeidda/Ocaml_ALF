type prof = Bartoletti | Pinna | Scateni | Carta ;;
type item = Pelo | Capello | Sputo |
Gesso | Pennarello | Cancellino | Notebook ;;
type tipo = Corpo | Lavoro ;;
type reperto = prof * item ;;

let get_tipo i = match i with
  i when (i = Pelo || i = Capello || i = Sputo) -> Corpo
  | _ -> Lavoro;;

let is_vodoo a b = match (a,b) with
  ((a,a'),(b,b')) when a=b && (get_tipo a') <> (get_tipo b') -> true
  | _ -> false;;

let count1 p t l = List.fold_left (fun x y -> x + 
(if fst(y) = p && get_tipo(snd(y)) = t then 1 else 0)) 0 l;;

let count2 p l = min (count1 p Corpo l) (count1 p Lavoro l);;

let count3 l = [(Bartoletti, count2 Bartoletti l),(Pinna, count2 Pinna l)
,(Scateni, count2 Scateni l), (Carta, count2 Carta l)];;


let rec get p t  = function
[] -> failwith "errore"
| (x,y)::l' when x = p && get_tipo(y) = t -> ((x,y),l')
| _::l' -> get p t l';;

let make_voodoo p l = 
  ((p,snd(fst(get p Corpo l)), snd(fst(get p Lavoro l))),
List.filter (fun x -> fst(get p Corpo l) <> x && fst(get p Lavoro l) <> x) l);;
(* List.append to append two list together *)
let rec append l1 l2 = 
  match l1 with
    | [] -> l2
    | hd :: tl -> hd :: append(tl l2)
;;

(* merging two lists into a list of pairs *)
let rec combine l1 l2 =
  match l1, l2 with
    | [], [] -> []
    | hd1::tl1, hd2::tl2 -> hd1,hd2::(combine tl1 tl2)
;;

(* split a list of pairs into two lists *)
let rec split l1 =
  match l1 with
  | [] -> [],[]
  | hd::tl -> (append [fst hd] (split tl)),(append [snd hd] (split tl))
;;

(* flatten a list of lists into a single list *)
let rec concat xss =
  match xss with
  | [] -> []
  | hd::tl -> hd @ (concat tl)
;;

(* returns the n-th element of a list *)
let nth xs n =
  let rec aux xs n =
    match xs with
    | [] -> []
    | hd::tl -> if n=0 then hd else aux tl (n-1) in
  if n<0 then raise (Invalid_argument "List.nth") else aux xs n
;;

let rec rev xs =
  match xs with
  | [] -> []
  | x::xs -> append (rev xs) [x]

let rec rev2 xs =
  let rec aux xs acc =
    match xs with
    | [] -> acc
    | x::xs -> aux xs (x::acc) in
  aux xs []
;;

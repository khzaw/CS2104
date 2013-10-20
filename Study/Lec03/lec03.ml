(* Recursive summation *)
let rec sum xs =
  match xs with
  | [] -> 0
  | x::xs -> x + sum(xs)
;;

(* Recursive production *)
let rec prod xs =
  match xs with
  | [] -> 1
  | x::xs -> x * prod(xs)
;;

(* Auxiliary function with accumulating parameter *)
let sum2 xs =
  let rec aux xs acc = 
    match xs with
    | [] -> acc
    | y::ys -> aux ys (acc+y)
  in aux xs 0
;;

(* Inefficient fibonacci *)
let rec fib n =
  if n<=1 then 1
  else (fib (n-1)) + (fib (n-2))
;;

(* Efficient fibonacci *)
let fib2 n =
  let rec aux n =
    if n<=0 then (1,0)
    else let (a,b) = aux(n-1) in (a+b, a)
    in fst(aux n)
;;

(* Efficient fibonacci tail-recursive *)
let fib3 n =
  let rec aux n a b =
    if n<=0 then a
    else aux(n-1) (a+b) a
    in aux n 1 0
;;

print_int (fib 4);;
print_int (fib2 4);;
print_int (fib3 4);;

(* Higher-Order Functions *)
let add x = fun y -> x + y;; (* is equivalent to *) let add x y = x + y;;
let add_mag x =
  if x >= 0 then fun y -> x+y
  else fun y -> -x + y;;
let inc = add 1;;
let inc10 = add 10;;
let dec = add (-1);;
let two : int = inc 1;;

(* Recursive summation/production can be generalized to *)
let rec fold f s xs =
  let rec aux xs =
    match xs with
    | [] -> s
    | hd::tl -> f hd (aux tl)
  in aux xs
;;


(* Transform one list to another *)
let map f xs =
  let rec aux xs =
    match xs with
    | [] -> []
    | hd::tl -> (f hd)::(aux tl)
  in aux xs
;;
let double xs = map (fun x-> 2*x) xs;;
let is_pos xs = map (fun x-> x>0) xs;;

(* Select a sublist that satisfies a given predicate *)
let filter p xs =
  let rec aux xs =
    match xs with
    | [] -> []
    | hd::tl -> if(p hd) then hd::(aux tl) else aux tl
    in aux xs
;;


let fold_left f z xs =
  let rec aux acc xs =
    match xs with
    | [] -> acc
    | hd::tl -> aux (f hd acc) tl
    in aux z xs
;;

(* general compose operator *)
let compose g f x = g (f x);;

(* Infix pipe operator *)
let (|>) (x:'a) (f:'a->'b) : 'b = f x;;

let double xs = List.map (fun x -> 2*x) xs;;
let sum xs = List.fold_left (+) 0 xs;; 


let pr_id x = x;;
let pr_pair pr1 pr2 (x,y) = "(" ^ (pr1 x) ^ "," ^ (pr2 y) ^ ")";;
let pr_triple pr1 pr2 pr3 (xy,yz) = "(" ^ (pr1 x) ^ "," ^ (pr2 y) ^ "," ^ (pr3 z) ^ ")";

(*let pr_list (pr:'a -> string) (xs:'a list) : string =*)
   (*"[" ^ (xs |> List.map pr |> String.concat ";") ^ "]"*)
(*;;*)

(*let wrapper (pre: 'a -> 'v) (post: 'v -> 'b -> unit)*)
            (*(post_exec: v->exn->unit)*)
            (*(f:'a->'b) (x:'a) : 'b =*)
  (*let v = pre x in*)
  (*try*)
    (*let r  = f x in*)
    (*let () = post v r in*)
    (*r*)
  (*with e ->*)
    (*let () = post_exc e r in*)
    (*raise e*)
(*;;*)


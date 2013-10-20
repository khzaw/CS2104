module Basic =
(* basic utilities that can be opened *)
struct

  let string_of_pair (p1:'a->string) (p2:'b->string) ((a,b):'a * 'b) : string = 
    "("^(p1 a)^","^(p2 b)^")"

  let rec remove_dups n = 
    match n with
        [] -> []
      | q::qs -> if (List.mem q qs) then remove_dups qs else q::(remove_dups qs)

  let pr_id x = x
  let pr_string x = "\""^x^"\""
  
  let print_endline_if b s = if b then print_endline s else ()
  let print_string_if b s = if b then print_string s else ()

  let print_flush s = print_endline (s); flush stdout

  let pr_no x = "?"
  let pr_none x = "?"

  let pr_unit x = "()"

  let pr_option f x = match x with
    | None -> "None"
    | Some v -> "Some("^(f v)^")"

  let pr_opt = pr_option 

  let pr_opt_int = pr_option string_of_int

  let pr_pair f1 f2 (x,y) = "("^(f1 x)^","^(f2 y)^")"

  let pr_triple f1 f2 f3 (x,y,z) = "("^(f1 x)^","^(f2 y)^","^(f3 z)^")"

  let pr_quad f1 f2 f3 f4 (x,y,z,z2) = "("^(f1 x)^","^(f2 y)^","^(f3 z)^","^(f4 z2)^")"
  let pr_penta f1 f2 f3 f4 f5 (x,y,z,z2,z3) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^")"
  let pr_hexa f1 f2 f3 f4 f5 f6 (x,y,z,z2,z3,z4) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^",6:"^(f6 z4)^")"

  let pr_hepta f1 f2 f3 f4 f5 f6 f7 (x,y,z,z2,z3,z4,z5) = "("^(f1 x)^",2:"^(f2 y)^",3:"^(f3 z)^",4:"^(f4 z2)^",5:"^(f5 z3)^",6:"^(f6 z4)^",7:"^(f7 z5)^")"

  let pr_quad_ln f1 f2 f3 f4 (x,y,z,z2) = "("^(f1 x)^"\n,2:"^(f2 y)^"\n,3:"^(f3 z)^"\n,4:"^(f4 z2)^")"

  let pr_lst s f xs = String.concat s (List.map f xs)
  let pr_list_brk_sep open_b close_b sep f xs  = open_b ^(pr_lst sep f xs)^close_b
  let pr_list_brk open_b close_b f xs  = pr_list_brk_sep open_b close_b "," f xs
  let pr_list f xs = pr_list_brk "[" "]" f xs
  let pr_list_ln f xs = "["^(pr_lst ",\n" f xs)^"]"
  let pr_list_mln f xs = (pr_lst "\n--------------\n" f xs)

  let add_str s f xs = s^":"^(f xs)

 let explode (s:string) : char list =
   let rec expl i l =
     if i < 0 then l else
       expl (i - 1) (s.[i] :: l) in
   expl (String.length s - 1) [];;

 let implode (l:char list) : string =
   let result = String.create (List.length l) in
   let rec imp i = function
     | [] -> result
     | c :: l -> result.[i] <- c; imp (i + 1) l in
   imp 0 l;;

end;;

exception Stack_Error

class ['a] stack  =
object (self)
  val mutable stk = []
  method push (i:'a) =
    begin
      stk <- i::stk
    end
  method get_stk  = stk (* return entire content of stack *)
  method set_stk newstk  = stk <- newstk
     (* override with a new stack *)
  method pop = match stk with
  | [] -> print_string "ERROR : popping empty stack";
    raise Stack_Error
  | x::xs -> stk <- xs
  method pop_top = match stk with
  | [] -> print_string "ERROR : popping empty stack";
    raise Stack_Error
  | x::xs -> stk <- xs; x
  method top : 'a = match stk with
  | [] -> print_string "ERROR : top of empty stack";
    raise Stack_Error
  | x::xs -> x
  method pop_no_exc = match stk with
  | [] -> ()
  | x::xs -> stk <- xs
  method is_empty = stk == []
  method is_avail = not(stk == [])
  method get = self # top
     (* method set x = self # push x *)
  method len = List.length stk
  method reverse = stk <- List.rev stk
  method reverse_of = List.rev stk
  method mem (i:'a) = List.mem i stk
  method mem_eq eq (i:'a) = List.exists (fun b -> eq i b) stk
     (* method exists (i:'a) = List.mem i stk  *)
     (* method exists_eq eq (i:'a) = List.exists (fun b -> eq i b) stk  *)
  method exists f = List.exists f stk
  method push_list (ls:'a list) =  stk <- ls@stk
  method pop_list (ls:'a list) =
    let rec drop n l =
      if n<=0 then l else
        match l with
        | h::t -> (drop (n-1) t)
        | [] -> []
    in stk <- drop (List.length ls) stk
  method reset = stk <- []
  method clone =
    Oo.copy self
   (* let n = new Gen.stack in *)
   (*   let lst = self # get_stk in *)
   (*   let _ = n # push_list lst in *)
   (* n *)
end;;

class ['a] stack_pr (epr:'a->string) (eq:'a->'a->bool)  =
object
  inherit ['a] stack as super
  val elem_pr = epr
  val elem_eq = eq
  method string_of = Basic.pr_list_ln elem_pr stk
  method string_of_no_ln = Basic.pr_list elem_pr stk
  method string_of_no_ln_rev =
    let s = super#reverse_of in
    Basic.pr_list elem_pr s
  method string_of_reverse =
    let s = super#reverse_of  in
    Basic.pr_list_ln elem_pr s
  method string_of_reverse_log =
    let s = super#reverse_of  in
    Basic.pr_list_mln elem_pr s
  method mem (i:'a) = List.exists (elem_eq i) stk
  method overlap (ls:'a list) =
    if (ls == []) then false
    else List.exists (fun x -> List.exists (elem_eq x) ls) stk
end;;

class ['a] stack_noexc (x_init:'a) (epr:'a->string) (eq:'a->'a->bool)  =
object
  inherit ['a] stack_pr epr eq
  val emp_val = x_init
  method top_no_exc : 'a = match stk with
  | [] ->  emp_val
  | x::xs -> x
  method last : 'a = match stk with
  | [] -> emp_val
  | _ -> List.hd (List.rev stk)
  method pop_top_no_exc = match stk with
  | [] -> emp_val
  | x::xs -> stk <- xs; x
end;;

class counter x_init =
object
  val mutable ctr = x_init
  method get : int = ctr
  method inc = ctr <- ctr + 1
  method inc_and_get = ctr <- ctr + 1; ctr
  method add (i:int) = ctr <- ctr + i
  method reset = ctr <- 0
  method string_of : string= (string_of_int ctr)
end;;

module StackTrace =
struct 
  (* keep track of calls being traced by ho_debug *)
  let ctr = new counter 0
    
  (* type stack = int list *)
  (* stack of calls being traced by ho_debug *)
  let debug_stk = new stack_noexc (-2) string_of_int (=)

  let dd_stk = new stack

  (* let force_dd_print () = *)
  (*   let d = dd_stk # get_stk in *)
  (*   debug_stk # overlap d *)

  let is_same_dd_get () =
    if dd_stk # is_empty then None
    else 
      let v1 = dd_stk # top in
      let v2 = debug_stk # top in
       (* let l1 = dd_stk # get_stk in *)
       (* let l2 = debug_stk # get_stk in *)
       (* let pr = Basic.pr_list string_of_int in *)
       (* let _ = print_endline ("ddstk:"^(pr l1)^" hostk:"^(pr l2)) in  *)
      if (v1==v2) then Some v1 else None

  let is_same_dd () =
    match (is_same_dd_get()) 
    with | None -> false
    | _ -> true

  (* pop last element from call stack of ho debug *)
  let pop_call () = 
    if is_same_dd () then dd_stk # pop;
    debug_stk # pop

  (* call f and pop its trace in call stack of ho debug *)
  let pop_aft_apply_with_exc (f:'a->'b) (e:'a) : 'b =
    let r = (try 
               (f e)
      with exc -> (pop_call(); raise exc))
    in pop_call(); r

  (* call f and pop its trace in call stack of ho debug *)
  let pop_aft_apply_with_exc_no (f:'a->'b) (e:'a) : 'b =
    try 
      let r = (f e) in
      (* debug_stk # pop;  *)
      r
    with exc -> ((* debug_stk # pop;  *)raise exc)


  (* string representation of call stack of ho_debug *)
  let string_of () : string =
    let h = debug_stk#get_stk in
    (* ("Length is:"^(string_of_int (List.length h))) *)
    String.concat "@" (List.map string_of_int (List.filter (fun n -> n>0) h) )

  let push_no_call () =
    ()
  (* debug_stk # push (-3) *)

  (* returns @n and @n1;n2;.. for a new call being debugged *)
  let push_call_gen (os:string) (flag:bool) : (string * string) = 
    ctr#inc;
    let v = ctr#get in
    debug_stk#push v; if flag then dd_stk#push v;
    let s = os^"@"^(string_of_int v) in
    let h = os^"@"^string_of() in
    (* let _ = print_endline ("push_call:"^os^":"^s^":"^h) in  *)
    s,h

  let push_call (os:string) : (string * string) = 
    push_call_gen os false

  let push_call_dd (os:string) : (string * string) = 
    push_call_gen os true

end;;

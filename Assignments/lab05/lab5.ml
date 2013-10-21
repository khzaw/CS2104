(* Lab 5 : Call-by-Name and Call-by-Value Lambda Interpreters *)
(* 

*)
open Globals;;

module Keyword =
struct
  (* permitted symbols *)
  let symbols = ["=";".";"(";")";"\\"];;
  (* reserved keywords in calculator *)
  let alphas = ["let";"in";"end"];;
end

module Lexical =
struct
  open Gen.Basic
  (* permitted token types *)
  type token = Key of string (* reserved keyword/symbol *)
               | Id of string  (* identifier *)
               | NumI of Num.num;; (* numeric constant *)
  exception LexErr of string;;

  (* printer for token *)
  let string_of_token t =
    match t with
      | Key s -> "Key "^s
      | Id s -> "Id "^s
      | NumI n -> "NumI "^(Num.string_of_num n)

  (* letters *)
  let is_letter c =
    'A'<= c && c <= 'Z'
    || 'a'<= c && c <= 'z';;

  (* digits *)
  let is_digit c =
    '0'<= c && c <= '9';;

  (* letters or digits *)
  let is_letter_or_digit c =
    is_letter c || is_digit c;;

  (* special symbols permitted *)
  let specials = explode "!@#$%^&*()+-=[]:/\"|;'\\,./?`_~<>";;

  (* alphanumeric *)
  (* pre: id starts with an alphabet *)
  let rec alphanum (cs:char list) (id:char list) : (string * char list) =
    match cs with
      | c::cs2 ->
            if is_letter_or_digit c 
            then alphanum cs2 (c::id)
            else (implode (List.rev id),cs)
      | [] -> (implode (List.rev id),[]);;
  let alphanum (cs:char list) (id:char list) =
    Debug.no_2 "alphanum"
        implode implode 
        (pr_pair pr_id implode) 
        alphanum cs id

  (* TO BE IMPLEMENTED *)
  (* numeric integer of arbitrary precision *)
  (* type: char list -> char list -> Num.num * char list *)
  let rec numeric (cs:char list) (digits:char list) =
    match cs with
      | c::cs2 ->
            if is_digit c 
            then numeric cs2 (c::digits)
            else (Num.num_of_string (implode (List.rev digits)),cs)
      | [] -> (Num.num_of_string (implode (List.rev digits)),[]);;

  let numeric (cs:char list) (digits:char list) =
    Debug.no_2 "numeric" 
        (add_str "cs" implode) implode 
        (pr_pair Num.string_of_num implode) 
        numeric cs digits

  let tokenof a = if List.mem a Keyword.alphas then Key a else Id a;;

  (* symbolic keyword *)
  let rec symbolic (cs:char list) (sy:char list) : string * (char list) =
    let ss = implode (List.rev sy) in
    match cs with
      | c::cs2 ->
            if List.mem ss Keyword.symbols 
            then (ss,cs)
            else 
              if not(List.mem c specials)
              then raise (LexErr ("Unrecognized lex symbol "^ss))
              else symbolic cs2 (c::sy)
      | [] -> (ss,[]);;

  (* lexical scanner *)
  (* type: char list -> token list -> token list *)
  let rec scanning cs toks =
    match cs with
      | c::cs -> 
            if is_letter c then
              let (id,cs2) = alphanum cs [c] in
              scanning cs2 (tokenof id::toks)
            else if is_digit c then
              let (id,cs2) = numeric cs [c] in
              scanning cs2 (NumI id::toks)
            else if List.mem c specials then
              let (sy,cs2) = symbolic cs [c]
              in scanning cs2 (Key sy:: toks)
            else 
              (* skip spaces, line breaks, strange chars *)
              scanning cs toks
      | [] -> 
            List.rev toks
  let scanning cs toks =
    Debug.no_1 "scanning" (pr_list Char.escaped) pr_none (fun _ -> scanning cs toks) cs

  (* type: string -> token list *)
  let scan a = scanning (explode a) []

end

module Parser =
struct
  open Lexical;;
  open Gen.Basic;;
  exception ParseErr of string;;
  type ('t,'r) parse = 't list -> 'r * 't list;; 
   type tokens = Lexical.token list;;
   let pr_toks = pr_list string_of_token

  (* phrase consisting of keyword *)
  (* type: string -> Lexical.token list -> unit * Lexical.token list *)
  let symbol (s:string) (toks:tokens) : string * tokens =
    let exc () = (ParseErr ("Symbol "^s^" expected.")) in
    match toks with
      | (Key b)::toks -> 
            if s=b then (b,toks) 
            else raise (exc ()) 
      | _ -> raise (exc ())

  let symbol (s:string) (toks:tokens) =
    Debug.no_2 "symbol" 
        pr_id (pr_list string_of_token) 
        (fun (_,t) -> pr_toks t)
        symbol s toks

  (* type: Lexical.token list -> string * Lexical.token list *)
  let ident (toks:tokens) : string * tokens =
    match toks with
      | Id a::toks -> (a,toks)
      | _ -> raise (ParseErr ("Identifier expected"))

  let ident (toks:tokens) =
    Debug.no_1 "ident" 
        (pr_list string_of_token) 
        (pr_pair pr_id pr_toks)
        ident toks

  (* type: Lexical.token list -> Num.num * Lexical.token list *)
  let numb (toks:tokens) : Num.num * tokens =
    match toks with
      | NumI a::toks -> (a,toks)
      | _ -> raise (ParseErr ("Integer expected"))
  let numb (toks:tokens) : Num.num * tokens =
    Debug.no_1 "numb" 
        (pr_list string_of_token) 
        (pr_pair Num.string_of_num pr_toks)
        numb toks

   (* type: (tokens -> 'a * tokens) -> ('a -> 'b) -> Lexical.token list -> 'b * tokens *)
   let (>>) (ph:('t,'a) parse) 
         (f:'a->'b)  
         (toks:'t list) : 'b*('t list) =
     let (x,toks2) = ph toks in
     (f x,toks2);;

   let (|%|) (ph1:('t,'a) parse)
         (ph2:('t,'a) parse)
         (toks:'t list) : 'a * ('t list)=
     try
       ph1 toks
     with (ParseErr _) -> ph2 toks;; 

   let (++) (ph1:('t,'a) parse)
         (ph2:('t,'b) parse)
         (toks:'t list) : ('a*'b)*('t list) =
     let (x,toks) = ph1 toks in
     let (y,toks) = ph2 toks in
     ((x,y),toks)

   let empty (toks:'t list) : 'a list * ('t list) = ([],toks)

   let rec repeat (ph:('t,'a) parse)
         (toks:'t list) : ('a list * 't list)  =
     ((ph ++ repeat ph >> (fun (a,b) -> a::b))
       |%| empty ) toks;;

   let expect (ph:('t,'a) parse) msg
         (toks:'t list) : ('a * 't list)  =
     try 
       ph toks
     with _ -> raise (ParseErr (msg));;

   let rec repeat1 (ph:('t,'a) parse)
         (toks:'t list) : ('a list * 't list)  =
     (ph ++ repeat ph >> (fun (a,b) -> a::b)) toks;;

   let rec option (ph:('t,'a) parse) 
         (toks:'t list) : ('a option * 't list)  
         = ( (ph >> (fun a -> Some a))
           |%| fun t -> (None,t)) toks;;


(* type: (tokens -> 'c * tokens) -> *)
(*   (string -> int) -> (string -> 'c -> 'c -> 'c) -> tokens -> 'c * tokens *)
   let infixes ph prec_of apply =
     let rec over k toks = next k (ph toks)
     and next k (x,toks) =
       match toks with
         | Key a :: toks2 ->
               if prec_of a < k then (x, toks)
               else next k ((over (prec_of a) >> apply a x) toks2)
         | _ -> (x,toks)
     in over 0;;

   (* more general reader *)
   let reader_gen ph scanner toks =
     match (ph (scanner toks)) with
       | (x,[]) -> x
       | (x,_::_) -> raise (ParseErr "Extra chars in input")

   (* type: (Lexical.token list -> 'p * 'q list) -> string -> 'p *)
   let reader (ph:tokens->'a*tokens) 
         (toks:string) : 'a =
     reader_gen ph scan toks

end

let usage_msg = Sys.argv.(0) ^ " --dre <method-name>"
let set_source_file arg = 
  Globals.source_files := arg :: !Globals.source_files
let process_cmd_line () = 
	Arg.parse Scriptarguments.common_arguments set_source_file usage_msg;;

process_cmd_line();
Debug.read_main();

module Lambda =
struct
  open Lexical;;
  open Gen.Basic;;
  (*
    <lambda> ::= <id>
             | <lambda> <lambda>
             | \ <id> . <lambda>
             | "(" <lambda> ")"
             | let <id>=<lambda> in <lambda>

  *)
  type lambda = 
           | Var of string
           | App of lambda * lambda
           | Lam of string * lambda
           | Let of string * lambda * lambda;;

  let string_of_lambda t =
    let rec aux t = match t with
           | Var x -> x
           | App(t1,t2) -> "("^aux t1^" "^(aux t2)^")"
           | Lam(i,t) -> "(\\"^i^"."^(aux t)^")"
           | Let(i,t1,t2) -> "let "^i^"="^(aux t1)^" in "^(aux t2)^" end"
    (* and aux_opt t =  *)
    (*   let s = aux t in *)
    (*   match t with *)
    (*     | App _ -> "("^s^")" *)
    (*     | _ -> s *)
    in aux t;;
  let sub lst i = List.filter (fun j -> not(i=j)) lst
  let remove_dupl xs =
    let rec aux xs lst = match xs with
           | [] -> List.rev lst
           | x::xs -> 
                 if List.mem x lst then aux xs lst
                 else aux xs (x::lst)
    in aux xs [];;

  let fv t =
    failwith "to be implemented from Lab4"

  let fv t = 
    Debug.no_1 "fv" string_of_lambda (pr_list pr_id) fv t

  let test t =
    print_endline (string_of_lambda t);
    print_endline (" has free vars "^(pr_list pr_id (fv t)));;

  let t1 = Lam("x",Var "y");;
  let t2 = Lam("y",t1);;
  let id = Lam("z",Var "z")
  let t3 = Lam("x",App(Var "x",Var "x"))
  let t3a = App(id,t3)
  let t4 = Lam("x",App(id,Var "x"))
  let t5 = App(t4,t3a);;
  (* ( λ x . (λ y . y) x) ((λ z. z) (λ x. x x) ) *)
  let t6a = App(Var "x",Var "y");;
  let t6c = App(Var "y",t6a);;
  let t7 = Let("y",id,t1);;
  let t7a = Let("z",id,t1);;

  open Parser;;

  let key_let = symbol "let";;
  let key_end = symbol "end";;
  let key_in = symbol "in";;
  let key_eq = symbol "=";;
  let key_lam = symbol "\\";;
  let key_dot = symbol ".";;
  let key_open = symbol "(";;
  let key_close = symbol ")";;

  let rec base_expr toks = 
    (
        (ident >> (fun s -> Var s))
      |%| ((key_open ++ (lam_expr++key_close)) >> (fun (_,(e,_)) -> e))
      |%| (failwith "to be implemented from Lab4")
    ) toks

  and lam_expr toks = 
    (
        repeat1 base_expr >> (fun app -> 
            match app with
              | [] -> failwith "not possible"
              | e::es -> List.fold_left (fun e n -> App(e,n)) e es
        )
    ) toks;;

  class name_generator pref =
  object 
    val mutable ctr = 0
    val prefix = pref
    method get_name : string = 
      let nm = prefix^"_"^(string_of_int ctr) in
      ctr <- ctr+1;
      nm
  end;;

  (* fresh name generator of form _n *)
  let fresh = new name_generator "";;

  let rec cnt_redex t = match t with
    | Var _ -> 0
    | Let _ -> 1
    | Lam _ -> 0
    | App(e1,e2) -> 
          match e1 with
            | Lam _ -> 1
            | _ -> (cnt_redex e1) + (cnt_redex e2);;

  let has_redex t = (cnt_redex t)>0

  let has_redex t = 
    Debug.no_1 "has_redex" string_of_lambda string_of_bool has_redex t

  let rename id id2 t2 =
    let rec aux t = match t with
          | Var j -> 
                if id=j then Var id2
                else t
          | Let(v1,t1,t2) ->
                failwith "rename to be implemented"
          | Lam(v1,t2) ->
                if v1=id then Lam(v1,t2)
                else Lam(v1,aux t2)
          | App(t1,t2) ->
                failwith "rename to be implemented"
    in aux t2
  let rename i j e = 
    Debug.no_2 "rename" (pr_pair pr_id pr_id) string_of_lambda string_of_lambda 
        (fun _ _ -> rename i j e) (i,j) e

  let subst id t1 t2 =
    let free_vars_t1 = fv t1 in
    let rec aux t =
      failwith "subst to be implemented"
    in aux t2

  let subst id t1 t2 =
    Debug.no_2 "subst" (pr_pair pr_id string_of_lambda) string_of_lambda string_of_lambda 
        (fun _ _ -> subst id t1 t2) (id,t1) t2


  (* reduce leftmost outermost redex *)
  (* tick to be invoked during beta-reduction *)
  let one_step_by_name tick t = 
    let rec aux t = match t with
      | Var _ -> t
      | Let (id,t1,t2) -> (tick(); subst id t1 t2)
      | Lam _ -> failwith "to be implemented"
      | App(e1,e2) -> failwith "to be implemented"
    in aux t;;

  let one_step_by_name tick t = 
    Debug.no_1 "one_step_by_name" string_of_lambda string_of_lambda 
        (fun _ -> one_step_by_name tick t) t

  (* reduce leftmost innermost redex *)
  (* tick to be invoked during beta-reduction *)
  let one_step_by_value tick t = 
    let rec aux t = 
      failwith "one_step_by_value to be implemented"
    in aux t;;

  let one_step_by_value tick t = 
    Debug.no_1 "one_step_by_value" string_of_lambda string_of_lambda 
        (fun _ -> one_step_by_value tick t) t

  let eval n one_step x =
    let ctr = new Gen.counter 0 in
    let tick () = ctr # inc in
    let step = one_step tick in
    let rec aux x =
      if has_redex x 
      then
        if ctr#get > n then
          failwith ("exceeded "^(string_of_int n)^" reductions")
        else
          aux (step x)
      else x
    in let r=aux x in
    (r,ctr#get);;

  let eval_by_name n x =
    eval n one_step_by_name x

  let eval_by_value n x =
    eval n one_step_by_value x

  let test_rename v1 v2 s =
    try
      let expr = (Parser.reader lam_expr s) in
      let n_expr = rename v1 v2 expr in
      print_endline ("Lambda.rename "^v1^" "^v2^" "^s^" => "^(string_of_lambda n_expr))
    with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

  let test_subst v1 t s =
    try
      let expr = (Parser.reader lam_expr s) in
      let t2 = (Parser.reader lam_expr t) in
      let n_expr = subst v1 t2 expr in
      print_endline ("Lambda.subst "^v1^" "^(string_of_lambda t2)^" "^s^" => "^(string_of_lambda n_expr))
    with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

  let test_eval s =
    try
      let (expr,steps) = eval_by_name 1000 (Parser.reader lam_expr s) in
      print_endline (s^" =eval("^(string_of_int steps)^")=> "^(string_of_lambda expr))
    with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

  let test_eval2 s =
    try
      let (expr,steps) = eval_by_value 1000 (Parser.reader lam_expr s) in
      print_endline (s^" =eval_val("^(string_of_int steps)^")=> "^(string_of_lambda expr))
    with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

  let test_lex s =
    try
      let tok = Lexical.scan s in
      print_endline (s^" =lex=> "^(pr_list Lexical.string_of_token tok))
    with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

  let test_parse s =
    try
      let expr = Parser.reader lam_expr s in
      print_endline (s^" =parse=> "^(string_of_lambda expr))
    with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

end;;

module L = Lambda;;
(* L.test L.t1;;  *)
(* L.test L.t2;;  *)
(* L.test L.t5;;  *)
(* L.test L.t6c;;  *)
(* L.test L.t7;;  *)
(* L.test L.t7a;;  *)


let s1 = "\\ x . x";;
let s2 = "\\ x . x x";;
let s3 = "x";;
let s4 = "x x (x y) z";;
let s5 = "\\ x . x \\ y. x";;
let s5a = "\\ x . x \\ y. x x";;
let s5b = "\\ x . x (\\ y. x) x";;
let s5c = "\\ x . \\y . x (\\ y. x) y";;
let s5d = "\\ x . (\\y . y (\\ y. x)) x";;
let s6 = "\\ x  y . y (\\ y. x) x";;
let s6a = "\\ . y (\\ x. x y) x";;
let s6c = "(\\ x y x.  x y (x x)) x";;
let s6d = "\\ x y x.  x y (x x) x";;
let s6e = "\\ y . (\\ x. x y) x";;

print_endline "=====================";;
print_endline "Testing for Renaming ";;
print_endline "=====================";;
L.test_rename "x" "_1" s4;; 
L.test_rename "z" "z2" s4;; 
L.test_rename "x" "z2" s5;; 
L.test_rename "x" "z2" s6e;; 
L.test_rename "y" "z2" s6e;; 

print_endline "========================";;
print_endline "Testing for Substitution ";;
print_endline "========================";;
L.test_subst "x" "z1 z2" s4;; 
L.test_subst "z" "z1 z2" s4;; 
L.test_subst "x" "z1 z2" s6e;; 
L.test_subst "x" "z y" s6e;; 
L.test_subst "y" "z1 z2" s6e;; 



(* print_endline "=====================";; *)
(* print_endline "Testing Lambda Parser";; *)
(* print_endline "=====================";; *)


(* (\* L.test_lex s1;;  *\) *)
(* (\* L.test_lex s2;;  *\) *)
(* L.test_parse s1;;  *)
(* L.test_parse s2;;  *)
(* L.test_parse s3;;  *)
(* L.test_parse s4;;  *)

(* L.test_parse s5;;  *)
(* L.test_parse s5a;;  *)
(* L.test_parse s5b;;  *)
(* L.test_parse s5c;;  *)
(* L.test_parse s5d;;  *)
(* L.test_parse s6;;  *)
(* L.test_parse s6a;;  *)
(* L.test_parse s6c;;  *)
(* L.test_parse s6d;;  *)

print_endline "===========================";;
print_endline "Testing Lambda Call-by-Name";;
print_endline "===========================";;
let s7 = "(\\ x  y . y x) (\\ y. y)";;
let s7a = "(\\ x  . x) (\\ y. y)";;
let s8 = "(\\ x  . x x) (\\ y. y y)";;
let s8a = "(\\x. y) ((\\ x  . x x) (\\ y. y y))";;
let s8b = "let v = (\\ x  . x) x in v end";;
let s8c = "let v =((\\ x  . x x) (\\ y. y y)) in v end";;
let s8d = "let v = (\\ x  . x) in v end";;
let s8e = "let v = ((\\ x  . x x) (\\ y. y y)) in z end";;
let s9 = "let v = ((\\ x  . x x) (\\y. y)) in v v end";;
let s10 = "(\\ x  . (\\ y. y x)) y";;
L.test_eval s7a;; 
L.test_eval s7;; 
L.test_eval s8;; 
L.test_eval s8a;; 
L.test_eval s8b;; 
L.test_eval s8c;;
L.test_eval s8d;; 
L.test_eval s8e;; 
L.test_eval s9;; 
L.test_eval s10;; 

print_endline "============================";;
print_endline "Testing Lambda Call-by-Value";;
print_endline "============================";;
L.test_eval2 s7a;; 
L.test_eval2 s7;; 
L.test_eval2 s8;; 
L.test_eval2 s8a;; 
L.test_eval2 s8b;; 
L.test_eval2 s8c;;
L.test_eval2 s8d;; 
L.test_eval2 s8e;; 
L.test_eval2 s9;; 
L.test_eval2 s10;; 

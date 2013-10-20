(* Lab 3 : An Arithmetic Calculator with Let-Bound Identifiers *)
(* 
  Your task is to introduce three new features into our
  arithmetic calculator
  (a) support the tokenizer for numbers
  (b) support an integer negation operator ~
  (c) let x=e1 in e2 to support let-bound vars

Bonus Mark:
  (i) support a right associative power operator
        x ^ 2 where is to square a number
  (ii) support a post-fix ! operator for factorial 
  (iii) support a post-fix ++ operator for integer increment

*)

open Globals;;

module Keyword =
struct
  (* permitted symbols *)
  let symbols = ["=";"!";"+";"-";"*";"/";"~";"^";"(";")"];;
  (* reserved keywords in calculator *)
  let alphas = ["let";"in"];;
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
            else (implode (List.rev id) ,cs)
      | [] -> (implode (List.rev id),[]);;
  let alphanum (cs:char list) (id:char list) =
    Debug.no_2 "alphanum"
        implode implode 
        (pr_pair pr_id implode) 
        alphanum cs id

  (* TO BE IMPLEMENTED *)
  (* numeric integer of arbitrary precision *)
  (* type: char list -> char list -> Num.num * char list *)
  let rec numeric (cs:char list) (id:char list) : (Num.num * char list) =
    match cs with
    | c::cs2 ->
        if is_digit c
          then numeric cs2 (c::id)
          else (Num.num_of_string (implode (List.rev id)), cs)
    | [] -> (Num.num_of_string (implode (List.rev id)), [])
  ;;

  let numeric (cs:char list) (digits:char list) =
    Debug.no_2 "numeric" 
        (add_str "cs" implode) implode 
        (pr_pair Num.string_of_num implode) 
        numeric cs digits

  let tokenof a = if List.mem a Keyword.alphas then Key a else Id a;;

  (* symbolic keyword *)
  let rec symbolic (cs:char list) (sy:string) : string * (char list) =
    match cs with
      | c::cs2 ->
            if List.mem sy Keyword.symbols 
            then (if (List.mem (Char.escaped c) Keyword.symbols) &&
            ((Char.escaped c) = "+" && (sy="+")) then
              (sy^(Char.escaped c), cs2 )else (sy,cs))
            else 
              if not(List.mem c specials)
              then raise (LexErr ("Unrecognized lex symbol "^sy))
              else symbolic cs2 (sy^(Char.escaped c))
      | [] -> (sy,[]);;

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
              let (sy,cs2) = symbolic cs (Char.escaped c)
              in scanning cs2 (Key sy:: toks)
            else 
              (* skip spaces, line breaks, strange chars *)
              scanning cs toks
      | [] -> 
            List.rev toks

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

   let rec option (ph:('t,'a) parse) 
         (toks:'t list) : ('a option * 't list)  
         = ( (ph >> (fun a -> Some a))
           |%| fun t -> (None,t)) toks;;

   (* let infixes ph prec_of apply = *)
   (*   let rec over k toks = next k (ph toks)  *)
   (*   and next k (x,toks) = *)
   (*     match toks with *)
   (*       | Key a :: toks2 -> *)
   (*             if prec_of a < k then (x, toks) *)
   (*             else next k ((over (prec_of a) >> apply a x) toks2) *)
   (*       | _ -> (x,toks)  *)
   (*   in over 0;; *)

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


(* command argument flags *)
let common_arguments = [
  ("--trace-failure", Arg.Set Globals.trace_failure,
   "Enable trace all failure (and exception)");
  ("-debug", Arg.String (fun s ->
      Debug.z_debug_file:=s; Debug.z_debug_flag:=true),
   "Read from a debug log file");
  ("-debug-regexp", Arg.String (fun s ->
      Debug.z_debug_file:=("$"^s); Debug.z_debug_flag:=true),
   "Match logged methods from a regular expression");
  ("-dre", Arg.String (fun s ->
      Debug.z_debug_file:=("$"^s); Debug.z_debug_flag:=true),
   "Shorthand for -debug-regexp");
  ("-v", Arg.Set Debug.debug_on,
   "Verbose")
  ]

let usage_msg = Sys.argv.(0) ^ " --dre <method-name>"
let set_source_file arg = 
  Globals.source_files := arg :: !Globals.source_files
let process_cmd_line () = 
	Arg.parse common_arguments set_source_file usage_msg;;

process_cmd_line();
Debug.read_main();


module Calc =
struct
  open Gen.Basic;;
  open Parser;;
  open Num;;
  let key_let = symbol "let";;
  let key_in = symbol "in";;
  let key_eq = symbol "=";;
  let key_plus = symbol "+";;
  let key_div = symbol "/";;
  let key_times = symbol "*";;
  let key_minus = symbol "-";;
  let key_power = symbol "^";;
  let key_neg = symbol "~";;
  let key_open = symbol "(";;
  let key_fact = symbol "!";;
  let key_close = symbol ")";;
  let key_increment = symbol "++";;

  type exp = ENum of Num.num
             | EId of string
             | EPlus of exp * exp
             | EMinus of exp * exp
             | EDiv of exp * exp
             | ETimes of exp * exp
             | ELet of string * exp * exp
             | EPower of exp * exp
             | EFact of exp

  let rec string_of_exp e =
    match e with
      | ENum x -> Num.string_of_num x
      | EId x -> x
      | EPlus (e1,e2) -> "("^(string_of_exp e1)^"+"^(string_of_exp e2)^")"
      | EMinus (e1,e2) -> "("^(string_of_exp e1)^"-"^(string_of_exp e2)^")"
      | ETimes (e1,e2) -> "("^(string_of_exp e1)^"*"^(string_of_exp e2)^")"
      | EDiv (e1,e2) -> "("^(string_of_exp e1)^"/"^(string_of_exp e2)^")"
      | ELet (i,e1,e2) -> "let "^(i)^"="^(string_of_exp e1)^" in "^(string_of_exp e2)
      | EPower (e1,e2) -> "("^(string_of_exp e1)^"^"^(string_of_exp e2)^")"
      | EFact (e1) -> "(fact " ^ (string_of_exp e1) ^")"

  let prec_op s =
    if s="^" then 1
    else if s="+" || s="-" then 2
    else if s="*" || s="/" then 3
    else if s="!" then 4
    else 0;;

  let apply_op s e1 e2 =
    if s="+" then EPlus (e1,e2)
    else if s="-" then EMinus (e1,e2)
    else if s="*" then ETimes (e1,e2)
    else if s="/" then EDiv (e1,e2)
    else if s="^" then EPower (e1,e2)
    else if s="!" then EFact(e1)
    else raise (ParseErr ("unknown apply_op "^s));;

  (*
    base ::=  Num | id | factor | "(" expr ")" | "~" term | "let" id "=" term "in" term
    factor ::= base "*" factor | base "/" factor | base
    term ::= factor + term | factor


    <expr> ::= <number> | <identifier> | <expr> op <expr> | ( <expr> ) | ~<expr>
              | let <identifier> = <expr> in <expr> | <expr>! | <expr>++
    <op> ::= +|-|*|/|^
  *)

  let zero = ENum (num_of_int 0);;
  let one = ENum (num_of_int 1);;

  (* TO BE COMPLETED *)
  (* "~" term | "let" id "=" term "in" term *)
  let rec factor tok = 
    (
        (numb >> (fun x-> ENum x))
      |%| (ident >> (fun s -> EId s))
      |%| (key_open ++ expr2 ++ key_close >> (fun ((_,e),_) -> e))
      |%| (key_neg ++ term >> (fun (_,e) -> EMinus (zero, e)))
      |%| (key_let ++ ident ++ key_eq ++ expr2 ++ key_in ++ expr2 >> (fun
        (((((_,i),_),e1),_),e2) -> ELet(i,e1,e2)))

    ) tok 

  and fact tok =
    (
      (factor ++ repeat (key_increment |%| key_fact) >> (fun(e1, lst) -> List.fold_left (fun e
      op -> if (op="++") then EPlus(e, one) else EFact(e)) e1 lst))
    ) tok

  (* TO BE COMPLETED *)
  (* ADD factor "/" term  before factor *)
  and term tok = 
    (
        ((fact ++ repeat ((key_times |%| key_div) ++ fact)) 
        >> (fun (e1,lst) -> List.fold_left (fun e (op,e2) -> if op="*" then ETimes (e,e2) else EDiv(e,e2)) e1 lst))
    ) tok

  and expr2a tok = 
    (
        ((term ++ repeat ((key_plus |%| key_minus) ++ term)) 
        >> (fun (e1,lst) -> List.fold_left (fun e (op,e2) -> if op="+" then EPlus (e,e2) else EMinus(e,e2)) e1 lst))
    ) tok
  
  and expr2 tok = (* power parsing *)
    (
      (((expr2a ++ key_power ++ expr2) >> (fun ((e1,_),e2) -> EPower (e1,e2)))
      |%| (expr2a))
    ) tok
  ;;

  (* TO BE IMPLEMENTED *)
  (* type: string -> Num.num -> exp -> exp *)
  (* substitute all occurrences of v by r in e *)
  (* subs@7 *)
  (* subs inp1 :v *)
  (* subs inp2 :5 *)
  (* subs inp3 :let v=(v+1) in (v+v) *)
  (* subs@7 EXIT:let v=(5+1) in (v+v) *)
  let rec subs (v:string) (r:num) (e:exp) : exp =  match e with
    | ENum x -> ENum x
    | EId x -> if(x=v) then ENum r else EId x
    | ELet(x, e1, e2) -> if(x=v) then ELet(x, subs v r e1, e2) else ELet(x, subs
    v r e1, subs v r e2)
    | EPlus(e1, e2) -> EPlus(subs v r e1, subs v r e2)
    | EMinus(e1, e2) -> EMinus(subs v r e1, subs v r e2)
    | ETimes(e1, e2) -> ETimes(subs v r e1, subs v r e2)
    | EDiv(e1, e2) -> EDiv(subs v r e1, subs v r e2)
    | EPower (e1, e2) -> EPower(subs v r e1, subs v r e2)
    | EFact(e1) -> EFact(subs v r e1)

  let subs (v:string) (r:num) (e:exp) : exp =
    Debug.no_3 "subs" 
        pr_id string_of_num string_of_exp
        string_of_exp
        subs v r e

   (*x^n in Num form *)
  let power x n  = x **/ n;;
  (*;;*)

  (*let power x n = ((factor ++ repeat (key_power) ++ factor) >> ())*)


  (* n! is to return the factorial of n *)
  let fact n =
    let rec f n a =
      match n with
      0 -> a
    | _ -> f (n-1) (n*a)
    in
    f n 1;;

  (* TO BE COMPLETED *)
  (* EMinus, EDiv, ELet cases *)
  (* Hint : ELet can be implemented using subs *)
  let rec eval (e:exp) : Num.num =
    match e with
      | ENum v -> v
      | EId v -> failwith ("Var "^v^" has not been bound") 
      | EPlus (e1,e2) -> (eval e1) +/ (eval e2)
      | EMinus (e1,e2) -> (eval e1) -/ (eval e2)
      | ETimes (e1,e2) -> (eval e1) */ (eval e2)
      | EDiv (e1,e2) -> (eval e1) // (eval e2)
      | ELet (v,e1,e2) -> eval(subs v (eval e1) e2)
      | EPower (e1, e2) -> power (eval e1) (eval e2)
      | EFact (e1) -> Num.num_of_int(fact (Num.int_of_num(eval e1)))
  ;;
  let eval (e:exp) : num =
    Debug.no_1 "eval" 
        string_of_exp
        string_of_num 
        eval e
end;;

open Gen.Basic
open Calc;;
let s1 = "v+y+z";;
let s2 = "v+w*y";;
let s3 = "v/y";;
let s4 = "55+66*9";;
let s5 = "5+666*(2/4)";;
let s6 = "5*6+6/7";;
let s7 = "v*y+z";;
let s8 = "v+w*y";;
let s9 = "v*w*y";;
let s10 = "v-w-y";;
let s11 = "v-w+y";;
let s12 = "v+w-y";;
let s13 = "v^w^z";;
let s14 = "v+w!!";;
let t1 = "let v = 6 in v+5*v";;
let t2 = "let v = 6 in let w = v+2 in w+5*v";;
let t3 = "let v = 6 in v + 5 / v";;
let t4 = "let v = 6 in v * 5 + x";;
let t5 = "let v = 5 in v * v";;
let t6 = "let v = 5 in let v = v+1 in v + v";;
let t7 = "2^3^2"
let t8 = "2^2!!!"
let t9 = "2+3!!"
let t10 = "2*3!!"
let e1 = "5*6%6/7";;
let e2 = "2 + ";;
let e3 = "2 + +";;
let e4 = "let let";;
let e5 = "let v = 5 in v ~ v";;
(*let e6 = "2++";;*)
(*let e7 = "3++!";;*)
(*let e8 = "3!++";;*)

let test_lex s =
  try
    let tok = Lexical.scan s in
    print_endline (s^" =lex=> "^(pr_list Lexical.string_of_token tok))
  with e -> print_endline ((Printexc.to_string e)^" encoutered for "^s);;

print_endline "=================";;
print_endline "Testing for Lexer";;
print_endline "=================";;
test_lex s1;;
test_lex s2;;
test_lex s3;;
test_lex s4;;
test_lex s5;;
test_lex s12;;
test_lex s13;;
test_lex s14;;
test_lex t1;;
test_lex t2;;
test_lex t3;;
test_lex t4;;
test_lex t5;;
test_lex t6;;
test_lex e1;;
test_lex e2;;
test_lex e3;;
test_lex e4;;
test_lex e5;;
(*test_lex e6;;*)
(*test_lex e7;;*)
(*test_lex e8;;*)

let test_parse s =
  try
    let expr = Parser.reader expr2 s in
    print_endline (s^" =parse=> "^(string_of_exp expr))
  with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

print_endline "==================";;
print_endline "Testing for Parser";;
print_endline "==================";;
test_parse s1;;
test_parse s2;;
test_parse s3;;
test_parse s4;;
test_parse s5;;
test_parse s6;;
test_parse s7;;
test_parse s8;;
test_parse s9;;
test_parse s10;;
test_parse s11;;
test_parse s12;;
test_parse s13;;
test_parse s14;;
test_parse t1;;
test_parse t2;;
test_parse t3;;
test_parse t4;;
test_parse t5;;
test_parse t6;;
test_parse t7;;
test_parse t8;;
test_parse t9;;
test_parse t10;;
(* test_parse e1;; *)
test_parse e2;;
test_parse e3;;
test_parse e4;;
test_parse e5;;
(*test_parse e6;;*)
(*test_parse e7;;*)
(*test_parse e8;;*)

let test_eval s =
  try
    let expr = Parser.reader expr2 s in
    print_endline (s^" =eval=> "^(Num.string_of_num (eval expr)))
  with e -> print_endline ((Printexc.to_string e)^" encountered for "^s);;

print_endline "=====================";;
print_endline "Testing for Evaluator";;
print_endline "=====================";;
test_eval s1;;
test_eval s2;;
test_eval s3;;
test_eval s4;;
test_eval s5;;
test_eval s6;;
test_eval t1;;
test_eval t2;;
test_eval t3;;
test_eval t4;;
test_eval t5;;
test_eval t6;;
test_eval t7;;
test_eval t8;;
test_eval t9;;
test_eval t10;;
(* test_eval e1;; *)
(* test_eval e2;; *)
(* test_eval e3;; *)
(* test_eval e4;; *)
(* test_eval e5;; *)
(*test_eval e6;; *)
(*test_eval e7;; *)
(*test_eval e8;; *)

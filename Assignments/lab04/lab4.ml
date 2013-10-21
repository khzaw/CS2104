(* Lab 4 : A Parser for Lambda Calculus *)
(* 

  The deadline for this lab is strict. You have
  to complete it by 22nd Oct 2013 8pm (Tue).

  Please start early.

  Your task is the following
  (i) build a parser for lambda calculus
      with a non-recursive let construct
  (ii) your parser must support multiple-argument
      lambda function of the form e.g. (\ x y z . x z)
      which would get translated to (\x .(\y.(\z. (x y))))
  (iii) Complete the implementation of the "fv" function which
     would compute a list of free variables for a given lambda expression.

*)
open Globals;;

module Keyword =
struct
  (* permitted symbols *)
  let symbols = ["=";".";"(";")";"\\"];;
  (* reserved keywords in calculator *)
  let alphas = ["let";"in"];;
end

module Lexical =
struct
  open Gen.Basic
  (* permitted token types *)
  type token = Key of string (* reserved keyword/symbol *)
               | Id of string  (* identifier *)
   exception LexErr of string;;

  (* printer for token *)
  let string_of_token t =
    match t with
      | Key s -> "Key "^s
      | Id s -> "Id "^s
      (* | NumI n -> "NumI "^(Num.string_of_num n) *)

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
            (* else if is_digit c then *)
            (*   let (id,cs2) = numeric cs [c] in *)
            (*   scanning cs2 (NumI id::toks) *)
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
   (* let infixes ph prec_of apply = *)
   (*   let rec over k toks = next k (ph toks) *)
   (*   and next k (x,toks) = *)
   (*     match toks with *)
   (*       | Key a :: toks2 -> *)
   (*             if prec_of a < k then (x, toks) *)
   (*             else next k ((over (prec_of a) >> apply a x) toks2) *)
   (*       | _ -> (x,toks) *)
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
    in aux t;;


  open Parser;;

  let key_let = symbol "let";;
  let key_in = symbol "in";;
  let key_eq = symbol "=";;
  let key_lam = symbol "\\";;
  let key_dot = symbol ".";;
  let key_open = symbol "(";;
  let key_close = symbol ")";;

(* Add two cases to based_expr to recorgniz *)
(* Namely (i)\ x1..xn -> e *)
(*    (ii) let v = x in e2 *)
  let rec base_expr toks = 
    (
        (ident >> (fun s -> Var s))
      |%| ((key_open ++ (lam_expr++key_close)) >> (fun (_,(e,_)) -> e))
      |%| ((key_lam ++ ( repeat1 ident  )++ key_dot ++ lam_expr) >> 
            (fun (((_,ixs),_),e) -> 
              List.fold_right (fun a b -> Lam(a,b) ) ixs e
            )
      )
    ) toks

(* 
   below already handles nested applications 
   in a left-associate manner
*)
  and lam_expr toks = 
    (
        repeat1 base_expr >> (fun app -> 
            match app with
              | [] -> failwith "empty application not possible"
              | e::es -> List.fold_left (fun e n -> App(e,n)) e es
        )
    ) toks;;


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

  let fv t =
    failwith "to be implemented (see sample trace"

  let fv t =
    Debug.no_1 "fv" string_of_lambda (pr_list pr_id)fv t
(*
Sample traces:

fv@1
fv inp1 :(\x.y)
fv@1 EXIT:[y]

fv@2
fv inp1 :(\y.(\x.y))
fv@2 EXIT:[]

fv@3
fv inp1 :((\x.((\z.z) x)) ((\z.z) (\x.(x x))))
fv@3 EXIT:[]

fv@4
fv inp1 :(y (x y))
fv@4 EXIT:[y,x]

fv@5
fv inp1 :let y=(\z.z) in (\x.y) end
fv@5 EXIT:[]

fv@6
fv inp1 :let z=(\z.z) in (\x.y) end
fv@6 EXIT:[y]

*)

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


end;;

module L = Lambda;;

print_endline "=====================";;
print_endline "Testing Lambda Parser";;
print_endline "=====================";;

let s1 = "\\ x . x";;
let s2 = "\\ x . x x";;
let s3 = "x";;
let s4 = "x x (x y) z";;

(* L.test_lex s1;;  *)
(* L.test_lex s2;;  *)
L.test_parse s1;; 
L.test_parse s2;; 
L.test_parse s3;; 
L.test_parse s4;; 

let s5 = "\\ x . x \\ y. x";;
let s5a = "\\ x . x \\ y. x x";;
let s5b = "\\ x . x (\\ y. x) x";;
let s5c = "\\ x . \\y . x (\\ y. x) y";;
let s5d = "\\ x . (\\y . y (\\ y. x)) x";;
L.test_parse s5;; 
L.test_parse s5a;; 
L.test_parse s5b;; 
L.test_parse s5c;; 
L.test_parse s5d;; 
let s6 = "\\ x  y . y (\\ y. x) x";;
let s6a = "\\ . y (\\ y. x) x";;
let s6c = "(\\ x y x.  x y (x x)) x";;
let s6d = "\\ x y x.  x y (x x) x";;
L.test_parse s6;; 
L.test_parse s6a;; 
L.test_parse s6c;; 
L.test_parse s6d;; 


print_endline "=====================";;
print_endline "Testing fv function";;
print_endline "=====================";;

L.test L.t1;; 
L.test L.t2;; 
L.test L.t5;; 
L.test L.t6c;; 
L.test L.t7;; 
L.test L.t7a;; 

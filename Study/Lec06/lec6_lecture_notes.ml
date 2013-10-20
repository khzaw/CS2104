(* creates a mutable reference ptr *)
	let ptr = ref 10;;
	let ptr2 = ref "hello";;

	(* prints content of ptr *)
	print_endline (string_of_int !ptr);;

	(* update the content of ptr *)
	ptr := !ptr + 1;;

	print_endline (string_of_int !ptr);;

	(* returns content of a mutable reference *)
	let v:int = !ptr;;
	let s:string = !ptr2;;

	let () = ptr := !ptr + 1;;
	let () = ptr2 := !ptr2 ^ " there!";;

(* declaring a record type *)
	type ('a, 'b) pair = 
	  { mutable first : 'a;
      second : 'b
    } ;;

	(* constructing a record value *)
	let p1 = {first = 1; second = "cs2104"};;

	print_endline ("First: "^(string_of_int p1.first));;
	print_endline ("Second: "^(p1.second));;

	(* update to mutable field *)
 	p1.first <- p1.first + 1;;


 	(* compile time error! *)
	(* Error: The record field label second is not mutable *)
	(* p1.second <- p1.second^" hello";; *)
	
	(*
	 type 'a ref = { mutable contents : 'a };;

 	let ref v = { contents = v };;

  let (!) r = r.contents;;

  let (:=) r v = r.contents <- v;
	*)
	
	let (p3:'a ref) = ref 1;;

  (* p3 := "hello";; *)

	let id x = x;;
	(* id: forall 'a. 'a -> 'a *)
	
	let p_id = ref id;;
	(* p_id : ('_a -> '_a) ref *)

  let v = !p_id 3;; 

	(* let s = !p_id "hello";; *)
	
	let memo =
    let cache = ref None in
    (fun x ->
       match !cache with
       | Some y -> y
       | None -> cache := Some x; x)
  ;;

  memo 3;; (* --> 3 *)

  memo 4;; (* --> 3 *)
 
	open Printf;;
	for i = 0 to 3 do printf "i = %d\n" i done;;

	for i = 3 downto 0 do printf "i = %d\n" i done;;

  let i = ref 3;;
	while (!i>=0) do
		printf "i = %d\n" !i;
		i := !i-1
	done;;

	let eval x = 
		printf "Elem %d \n" x; x in
	[eval 1; eval 2];;

(*
	type 'a dll = DNil
		| DCons of ('a dll) * 'a * ('a dll) 

	let cnv_to_dll xs =
  	let rec aux p xs =
   		match xs with
			| [] -> DNil
			| y::ys -> 
	  		let rec n = DCons (p,y,aux n ys)
       	in n
  	in aux DNil xs;; 
*)

	type 'a dll = DNil
		| DCons of 'a dnode
	 and 'a dnode  = 
			{ mutable left : ('a dll);
          content : 'a;
          mutable right : ('a dll)
        } 

	let cnv_to_dll xs =
  	let rec aux p xs =
   	match xs with
		| [] -> DNil
		| y::ys ->
			let n = {left=p;content=y; right=DNil} in
	  	let r = DCons n in
	  		(n.right <- aux r ys; r)
  	in aux DNil xs;; 

	let rec take n xs =
		match xs with
		| [] -> []
		| y::ys -> if n=0 then [] else y::(take (n-1) ys);;
 
	let rec cir_list = 1::2::cir_list;;

  take 5 cir_list;;

	let rec inf n = n::inf(n+1);;

	let e1 = lazy (print_endline "evaluating"; 1+4);;

	Lazy.force e1;;

	Lazy.force e1;;

	type 'a lazy_list = LNil
		| LCons of 'a * (('a lazy_list) Lazy.t);;

	let rec infL n = LCons (n, lazy (infL(n+1)));;

	let rec takeL n xs =
		match xs with
		| LNil -> []
		| LCons(y,ys) -> 
			if n=0 then [] 
			else y::(takeL (n-1) (Lazy.force ys));;

	takeL 5 (infL 1);;

  type 'a lazy_state =
		Delayed of (unit -> 'a)
		| Value of 'a
		| Exn of exn;;

	type 'a lazy_t = ('a lazy_state) ref;; 

  (* lazy e ==> ref (Delayed (fun () -> e)) *)
	
	let force e = match !e with
		| Delayed f -> 
			(try 
				let v = f () in (e:=Value v; v)
			with ex -> (e:=Exn ex; raise ex))
		| Value v -> v
		| Exn ex -> raise ex;;

	let test () =
		output_string stdout "What is your name?";
		flush stdout;
		let ans = input_line stdin in 
		   output_string stdout ("Hello "^ans^"\n");;

  let fmt = "%i is an integer, %F is a float, "
	     	       ^^"\"%s\" is a string\n";;

  let pr = Printf.printf fmt;;

  pr 3 4.5 "five";;

  

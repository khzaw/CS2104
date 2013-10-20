(* Imperative Programming *)

(* creates a mutable reference prt *)
let ptr = ref 10;;
print_endline (string_of_int !ptr);;

(* update the content of ptr *)
ptr := !ptr + 1;;
print_endline (string_of_int !ptr);;

let ptr2 = ref "hello";;

(* returns the content of a mutable reference *)
let v:int = !ptr;;
let s:string = !ptr2;;

(* updating a mutable reference *)
let () = ptr := !ptr + 1;;
let () = ptr2 := !ptr2 ^ "there!";;

(* Records are immutable are by default, but we can selectively provide fields
 * that are mutable *)

type ('a, 'b) pair = { mutable first: 'a; second: 'b };; (* first is mutable, second is not *)
let p1 = {first = 1; second = "cs2104"};;
print_endline ("First :" ^ (string_of_int p1.first));;
print_endline ("Second :" ^ (p1.second));;

(* Only mutable fields can be updated *)
p1.first <- p1.first + 1;;
(*p2.second <- p2.second ^ "ss";;*)
print_endline ("First :" ^ (string_of_int p1.first));;
print_endline ("Second :" ^ (p1.second));;

(* Implementation of Ref Type *)
  (* type declaration *)
(*
 *  type 'a ref = {mutable contents : 'a};;
 *
 *  [> construction <]
 *  let ref v = {contents = v};;
 *
 *  [> retrieval <]
 *  let (!) r = r.contents;;
 *
 *  [> update <]
 *  let (:=) r v = r.contents <- v;;
 *)

(* Weak polymorphism *)
  (* Types of mutable fields must not be polymorphic *)
  (*let (p4: ('a list) ref) = ref [];;*)
  (*p4 := 1::!p4;;*)
  (*p4 := "string"::!p4;; [> error <]*)

  (* Mutable reference must have a single type, so that update and retriveal
   * remains consistent *)
    (* Notice the CONSTRUCTION *)


  let (p3:'a ref) = ref 1;;
  (* Immutable values can be polymorphic *)
	let id x = x;;
	(* id: forall 'a. 'a -> 'a *)
	
	(*let p_id = ref id;;*)
	(* p_id : ('_a -> '_a) ref *)

  (* Mutable reference can only be monomorphic *)
  (*let v = !p_id 3;;*)
  (*let s = !p_id "hello";;*)

  let memo =
    let cache = ref None in
    (fun x ->
      match !cache with
      | Some y -> y
      | None -> cache := Some x; x);;

  memo 3;; (* --> 3 *)
  memo 4;; (* --> 4 *)

  (* Cache is now local to each call where a new cache is created for each call *)
  let memo2 =
    (fun x ->
      let cache = ref None in
        match !cache with
        | Some y -> y
        | None -> cache := Some x; x);;

(* I/O *)
  (* Buffered I/O library *)
  (* Interact with Terminal *)
  let test () =
    output_string stdout "What is your name? :";
    flush stdout;
    let ans = input_line stdin 
      in output_string stdout ("Hello " ^ ans ^ "!\n");;

  (* Output to a File *)
  let file = open_out "test.out";;    (* creates an out_channel file *)
  output_string file "Hello There!";; (* writes to file *)
  close_out file;; (* closes the file *)

  (* Formatted Printing *)
    let pr = Printf.printf
    ("%i is an integer, %F is a float" ^^ "\" %s\" is a string\n");;
    (* notice the double caret *)

    let fmt = ("%i is an integer, %F is a float" ^^ "\" %s\" is a string\n");;
    let pr = Printf.printf fmt;;
    pr 3 4.5 "five";;

  (* Loop Iterators *)
    open Printf;;
    (* For loop *)
      for i = 0 to 3 do printf "i = %d\n" i done;; (* i itself is local and immutble *)
      for i = 3 downto 0 do printf "i =%d\n" i done;; 

    (* While loop *)
    
    let i = ref 3;;
    while (!i >= 0) do
      printf "i = %d\n" !i;
      i := !i-1
    done;;

    let for_loop init final stmt =
      let rec aux i =
        if i <= final
          then (stmt (); aux (i+1))
      in aux init
    ;;

    let for_downto_loop init final stmt =
      let rec aux i =
        if i >= final
          then (stmt (); aux (i-1))
      in aux init
    ;;

(* Lazy Evaluation *)

  


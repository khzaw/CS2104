(* global types and utility functions *)

type loc =  {
    start_pos : Lexing.position (* might be expanded to contain more information *);
    mid_pos : Lexing.position;
    end_pos : Lexing.position;
  }

let no_pos =
  let no_pos1 = { Lexing.pos_fname = "";
		  Lexing.pos_lnum = 0;
		  Lexing.pos_bol = 0;
		  Lexing.pos_cnum = 0 } in
  {start_pos = no_pos1; mid_pos = no_pos1; end_pos = no_pos1;}

let is_no_pos l = (l.start_pos.Lexing.pos_cnum == 0)

let string_of_loc (p : loc) =
    Printf.sprintf "1 File \"%s\",Line:%d,Col:%d"
    p.start_pos.Lexing.pos_fname
    p.start_pos.Lexing.pos_lnum
    (p.start_pos.Lexing.pos_cnum-p.start_pos.Lexing.pos_bol)
;;

let string_of_pos (p : Lexing.position) =
    Printf.sprintf "(Line:%d,Col:%d)"
    p.Lexing.pos_lnum
      (p.Lexing.pos_cnum-p.Lexing.pos_bol)
;;

(*Proof logging facilities*)
class ['a] store (x_init:'a) (epr:'a->string) =
   object (self)
     val emp_val = x_init
     val mutable lc = None
     method is_avail : bool = match lc with
       | None -> false
       | Some _ -> true
     method set (nl:'a) = lc <- Some nl
     method get :'a = match lc with
       | None -> emp_val
       | Some p -> p
     method reset = lc <- None
     method get_rm :'a = match lc with
       | None -> emp_val
       | Some p -> (self#reset; p)
     method string_of : string = match lc with
       | None -> "Why None?"
       | Some l -> (epr l)
     method dump = print_endline ("\n store dump :"^(self#string_of))
   end;;

class prog_loc =
object
  inherit [loc] store no_pos string_of_loc
     method string_of_pos : string = match lc with
       | None -> "None"
       | Some l -> (string_of_pos l.start_pos)
end;;

(*Some global vars for logging*)
let post_pos = new prog_loc

let entail_pos = ref no_pos

let verbose_num = ref 0

let source_files = ref ([] : string list)

let trace_failure = ref false

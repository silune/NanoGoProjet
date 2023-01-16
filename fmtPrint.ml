open X86_64
open Tast

(* ----- auxilary functions ----- *)

let sizeof = Typing.sizeof
let new_print_label = 
  let r = ref 0 in fun () -> incr r; "L_SP_" ^ string_of_int !r

(* check if a type is reccursive, including pointer *)
let check_recursive_printing typ =
  let rec aux lst t =
    if List.mem t lst then
      true
    else
      match t with
      | Tstruct s ->
        Hashtbl.fold (fun key f res -> (aux (t :: lst) f.f_typ) || res) s.s_fields false
    | Tptr typ1 -> aux lst typ1
    | Tmany [typ1] -> aux lst typ1
    | _ -> false
  in aux [] typ

(* transform the Hashtbl of fields into a list sorted by declaration order *)
let fields_to_lst f_hashtbl =
  let sort_fun f1 f2 =
    if f1.f_order = f2.f_order then
      0
    else if f1.f_order > f2.f_order then
      1
    else
      -1
  in
  let lst = Hashtbl.fold (fun key f lst -> f :: lst) f_hashtbl [] in
  List.sort sort_fun lst

let rec is_struct = function
  | Tstruct _ -> true
  | Tmany [typ] -> is_struct typ
  | _ -> false
  
(* ----- constant ----- *)

let print =
  xorq (reg rax) (reg rax) ++
  call "printf" ++
  ret

let print_space =
  label "print_space" ++
  movq (ilab "S_space") (reg rdi) ++
  print

let data_print_space =
  label "S_space" ++ string " "

let print_lbra =
  label "print_lbra" ++
  movq (ilab "S_lbra") (reg rdi) ++
  print

let data_print_lbra =
  label "S_lbra" ++ string "{"

let print_rbra =
  label "print_rbra" ++
  movq (ilab "S_rbra") (reg rdi) ++
  print

let data_print_rbra =
  label "S_rbra" ++ string "}"

let print_esper =
  label "print_esper" ++
  movq (ilab "S_esper") (reg rdi) ++
  print

let data_print_esper =
  label "S_esper" ++ string "&"

let print_nil = 
  label "print_nil" ++
  movq (ilab "S_nil") (reg rdi) ++
  print

let data_print_nil =
  label "S_nil" ++ string "<nil>"

(* ----- print code memory ----- *)

let mem_print = Hashtbl.create 5
let mem_pointer = ref false

let print_functions = ref (
  print_space ++
  print_lbra ++
  print_rbra ++
  print_esper ++
  print_nil)

let print_data = ref (
  data_print_space ++
  data_print_lbra ++
  data_print_rbra ++
  data_print_esper ++
  data_print_nil)

(* ----- print function ----- *)

(* gives the label of a type in order to print it *)
let rec type_to_label = function
  | Tbool -> "print_bool"
  | Tint -> "print_int"
  | Tstring -> "print_string"
  | Tptr (Tstruct s) -> "print_struct_pointer_" ^ s.s_name
  | Tptr _ -> "print_pointer_hex"
  | Tstruct s -> "print_struct_" ^ s.s_name
  | Tmany [typ] -> type_to_label typ
  | typ -> Printf.printf "%s\n" (Typing.string_of_type typ); assert false (* impossible si bien typé *)

(* print all the fields *)
let rec print_field_lst = function
  | [] -> nop
  | [f] -> print_field f
  | f :: f_rest -> print_field f ++ call "print_space" ++ print_field_lst f_rest

(* print one field *)
and print_field f =
  pushq (reg rbx) ++
  movq (reg rbx) (reg rdi) ++
  addq (imm f.f_ofs) (reg rdi) ++
  (if not (is_struct f.f_typ) then
    movq (ind rdi) (reg rdi)
  else
    nop) ++
  call (type_to_label f.f_typ) ++
  popq rbx

(* add the print functions required (list of types) to the code *)
let rec require = function
  | [] -> ()
  | typ :: rest when Hashtbl.mem mem_print typ -> require rest
  | typ :: rest ->
      let is_rec = check_recursive_printing typ in
      (if is_rec then
        Printf.eprintf "\x1B[1;49;95mWarning\x1B[0m : risk of recursive printing, switching to simple print for %s\n" (Typing.string_of_type typ));
      Hashtbl.add mem_print typ is_rec;
      print_functions := !print_functions ++ (print_function_of typ is_rec);
      print_data := !print_data ++ (print_data_of typ);
      require rest

(* gives the code to print a type *)
and print_function_of typ recur =
  label (type_to_label typ) ++
  (match typ with
    | Tbool ->
        let l_false = new_print_label () in
        let l_end = new_print_label () in
        testq (reg rdi) (reg rdi) ++
        jz l_false ++
        movq (ilab "S_true") (reg rdi) ++
        jmp l_end ++
        label l_false ++
        movq (ilab "S_false") (reg rdi) ++
        label l_end ++
        print
    | Tint -> 
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_int") (reg rdi) ++
        print
    | Tstring -> 
        testq (reg rdi) (reg rdi) ++
        jz "print_nil" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_string") (reg rdi) ++
        print
    | Tptr (Tstruct s) when not recur ->
        require [Tstruct s];
        pushq (reg rdi) ++
        testq (reg rdi) (reg rdi) ++
        je "print_nil" ++
        call "print_esper" ++
        popq rdi ++
        call (type_to_label (Tstruct s)) ++
        ret
    | Tptr _ ->
        testq (reg rdi) (reg rdi) ++
        je "print_nil" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab "S_pointer_hex") (reg rdi) ++
        print
    | Tstruct s ->
        let l_fields = fields_to_lst s.s_fields in
        require (List.map (fun f -> f.f_typ) l_fields);
        movq (reg rdi) (reg rbx) ++
        call "print_lbra" ++
        print_field_lst l_fields ++
        call "print_rbra" ++
        ret
    | Tmany [typ] -> print_function_of typ recur
    | typ -> Printf.printf "%s\n" (Typing.string_of_type typ); assert false) (* impossible si bien typé *)

(* gives the data used to print a type *)
and print_data_of = function
  | Tbool ->
      label "S_true" ++ string "true" ++
      label "S_false" ++ string "false"
  | Tint ->
      label "S_int" ++ string "%ld"
  | Tstring ->
      label "S_string" ++ string "%s"
  | Tptr _ when not !mem_pointer ->
      mem_pointer := true;
      label "S_pointer_hex" ++ string "0x%x"
  | _ -> nop

(* ----- call function ----- *)

let print_one typ =
  require [typ];
  call (type_to_label typ)

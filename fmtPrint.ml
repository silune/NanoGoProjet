open X86_64
open Tast

let sizeof = Typing.sizeof

(* ----- functions ----- *)

let check_recursive_printing typ =
  let rec aux lst = function
    | Tstruct s ->
        Hashtbl.fold (run_field (typ :: lst)) s.s_fields false
    | Tptr typ1 -> aux lst typ1
    | Tmany [typ1] -> aux lst typ1
    | _ -> false

  and run_field lst key f res =
    if List.mem f.f_typ lst then
      true
    else (aux lst f.f_typ) || res

  in aux [] typ

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

let rec print_one_safe go = function
  | Tbool ->
      call "print_bool"
  | Tint ->
      call "print_int"
  | Tstring ->
      call "print_string"
  | Tptr (Tstruct s) when go ->
      movq (ind rdi) (reg rbx) ++
      testq (reg rdi) (reg rdi) ++
      je "print_nil" ++
      call "print_esper" ++
      print_struct go s
  | Tptr _ when go ->
      call "print_pointer_hex"
  | Tptr _ ->
      call "print_pointer"
  | Tstruct s ->
      movq (reg rdi) (reg rbx) ++
      print_struct go s
  | Tmany [typ] -> print_one_safe go typ
  | typ -> Printf.printf "%s\n" (Typing.string_of_type typ); assert false (* impossible si bien typé *)

and print_inside go typ = match typ with
  | Tstruct s ->
      pushq (reg rbx) ++
      movq (reg rax) (reg rbx) ++
      print_struct go s ++
      popq rbx
  | Tptr (Tstruct s) when go ->
      pushq (reg rbx) ++
      movq (ind rax) (reg rbx) ++
      testq (reg rbx) (reg rbx) ++
      je "print_nil" ++
      call "print_esper" ++
      print_struct go s ++
      popq rbx
  | Tbool | Tint | Tstring | Tptr _ ->
      movq (ind rax) (reg rdi) ++
      print_one_safe go typ
  | _ -> assert false (* impossible si bien typé *)

and print_field go f =
  movq (reg rbx) (reg rax) ++
  addq (imm f.f_ofs) (reg rax) ++
  print_inside go f.f_typ

and print_field_lst go fl = match fl with
  | [] -> nop
  | [f] -> print_field go f
  | f :: f_rest -> print_field go f ++ call "print_space" ++ print_field_lst go f_rest

and print_struct go s =
  call "print_lbra" ++
  print_field_lst go (fields_to_lst s.s_fields) ++
  call "print_rbra"

let print_one ?(go = false) typ =
  if (go && check_recursive_printing typ) then
    (Printf.eprintf("\x1B[1;49;95mWarning\x1B[0m : risk of recursive printing, switching to Simon's printing for one printing\n");
    print_one_safe false typ)
  else
    (print_one_safe go typ)

(* ----- constants ----- *)

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

let print_string =
  label "print_string" ++
  testq (reg rdi) (reg rdi) ++
  jz "print_nil" ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab "S_string") (reg rdi) ++
  print

let data_print_string =
  label "S_string" ++ string "%s"

let print_pointer =
  label "print_pointer" ++
  testq (reg rdi) (reg rdi) ++
  je "print_nil" ++
  jmp "print_int"

let print_pointer_hex =
  label "print_pointer_hex" ++
  testq (reg rdi) (reg rdi) ++
  je "print_nil" ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab "S_pointer_hex") (reg rdi) ++
  print

let data_print_pointer_hex =
  label "S_pointer_hex" ++ string "0x%x"

let print_nil = 
  label "print_nil" ++
  movq (ilab "S_nil") (reg rdi) ++
  print

let data_print_nil =
  label "S_nil" ++ string "<nil>"

let print_int =
  label "print_int" ++
  movq (reg rdi) (reg rsi) ++
  movq (ilab "S_int") (reg rdi) ++
  print

let data_print_int =
  label "S_int" ++ string "%d" 

let print_bool =
  label "print_bool" ++
  testq (reg rdi) (reg rdi) ++
  jz "l_false" ++
  movq (ilab "S_true") (reg rdi) ++
  jmp "l_end" ++
  label "l_false" ++
  movq (ilab "S_false") (reg rdi) ++
  label "l_end" ++
  print

let data_print_bool =
  label "S_true" ++ string "true" ++
  label "S_false" ++ string "false"

let print_functions = 
  print_space ++
  print_lbra ++
  print_rbra ++
  print_esper ++
  print_string ++
  print_pointer ++
  print_pointer_hex ++
  print_nil ++
  print_int ++
  print_bool

let print_data =
  data_print_space ++
  data_print_lbra ++
  data_print_rbra ++
  data_print_esper ++
  data_print_string ++
  data_print_pointer_hex ++
  data_print_nil ++
  data_print_int ++
  data_print_bool


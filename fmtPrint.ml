open X86_64
open Tast

let sizeof = Typing.sizeof

(* ----- functions ----- *)

let fields_to_lst f_hashtbl =
  Hashtbl.fold (fun key f lst -> f :: lst) f_hashtbl []

let rec print_one = function
  | Tbool -> call "print_bool"
  | Tint -> call "print_int"
  | Tstring -> call "print_string"
  | Tptr _ -> call "print_pointer"
  | Tstruct s -> movq (reg rdi) (reg rbx) ++ print_struct s
  | _ -> assert false (* impossible si bien typé *)

and print_inside typ = match typ with
  | Tstruct s ->
      pushq (reg rbx) ++
      movq (reg rax) (reg rbx) ++
      print_struct s ++
      popq rbx
  | Tbool | Tint | Tstring | Tptr _ ->
      movq (ind rax) (reg rdi) ++
      print_one typ
  | _ -> assert false (* impossible si bien typé *)

and print_field f =
  movq (reg rbx) (reg rax) ++
  addq (imm f.f_ofs) (reg rax) ++
  print_inside f.f_typ

and print_field_lst fl = match fl with
  | [] -> nop
  | [f] -> print_field f
  | f :: f_rest -> print_field_lst f_rest ++ call "print_space" ++ print_field f

and print_struct s =
  call "print_lbra" ++
  print_field_lst (fields_to_lst s.s_fields) ++
  call "print_rbra"

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
  print_string ++
  print_pointer ++
  print_nil ++
  print_int ++
  print_bool

let print_data =
  data_print_space ++
  data_print_lbra ++
  data_print_rbra ++
  data_print_string ++
  data_print_nil ++
  data_print_int ++
  data_print_bool


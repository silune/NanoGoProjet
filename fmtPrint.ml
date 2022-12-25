open X86_64

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
  movq (ind rdi) (reg rdi) ++
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
  print_string ++
  print_pointer ++
  print_nil ++
  print_int ++
  print_bool

let print_data =
  data_print_space ++
  data_print_string ++
  data_print_nil ++
  data_print_int ++
  data_print_bool


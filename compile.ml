(* étiquettes
     F_function      entrée fonction
     E_function      sortie fonction
     L_xxx           sauts
     S_xxx           chaîne

   expression calculée avec la pile si besoin, résultat final dans %rdi

   fonction : arguments sur la pile, résultat dans %rax ou sur la pile

            res k
            ...
            res 1
            arg n
            ...
            arg 1
            adr. retour
   rbp ---> ancien rbp
            ...
            var locales
            ...
            calculs
   rsp ---> ...

*)

open Format
open Ast
open Tast
open X86_64

exception Anomaly of string

let debug = ref false

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let set_up_div = movq (reg rdi) (reg rax) ++ cqto

let sizeof = Typing.sizeof

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}

let empty_env =
  { exit_label = ""; ofs_this = -1; nb_locals = ref 0; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

let rec expr env e = match e.expr_desc with
  | TEskip ->
    nop

  | TEconstant (Cbool true) ->
    movq (imm 1) (reg rdi)

  | TEconstant (Cbool false) ->
    movq (imm 0) (reg rdi)

  | TEconstant (Cint x) ->
    movq (imm64 x) (reg rdi)

  | TEnil ->
    xorq (reg rdi) (reg rdi)

  | TEconstant (Cstring s) ->
    (* TODO code pour constante string DONE *)
    let lab_s = alloc_string s in
    movq (ilab lab_s) (reg rdi)

  | TEbinop (Band, e1, e2) ->
    (* TODO code pour ET logique lazy DONE *)
    let l_end = new_label () in
    expr env e1 ++ testq (reg rdi) (reg rdi) ++
    jz l_end ++
    expr env e2 ++
    label l_end

  | TEbinop (Bor, e1, e2) ->
    (* TODO code pour OU logique lazy DONE *) 
    let l_end = new_label () in
    expr env e1 ++ testq (reg rdi) (reg rdi) ++
    jnz l_end ++
    expr env e2 ++
    label l_end

  | TEbinop (Blt | Ble | Bgt | Bge as op, e1, e2) ->
    (* TODO code pour comparaison ints DONE *)
    let comp_jmp = match op with
      | Blt -> jl
      | Ble -> jle
      | Bgt -> jg
      | Bge -> jge
      | _ -> assert false (* n'arrive jamais mais warning sinon *)
    in
      expr env e1 ++ pushq (reg rdi) ++
      expr env e2 ++ pushq (reg rdi) ++
      popq rsi ++ popq rdi ++
      cmpq (reg rsi) (reg rdi) ++
      compile_bool comp_jmp

  | TEbinop (Badd | Bsub | Bmul | Bdiv | Bmod as op, e1, e2) ->
    (* TODO code pour arithmetique ints - DONE *)
    let code_op = match op with
      | Badd -> addq (reg rsi) (reg rdi)
      | Bsub -> subq (reg rsi) (reg rdi)
      | Bmul -> imulq (reg rsi) (reg rdi)
      | Bdiv -> set_up_div ++ idivq (reg rsi) ++ movq (reg rax) (reg rdi)
      | Bmod -> set_up_div ++ idivq (reg rsi) ++ movq (reg rdx) (reg rdi)
      | _ -> assert false (* arrive jamais mais warning sinon *)
    in
      expr env e1 ++ pushq (reg rdi) ++
      expr env e2 ++ pushq (reg rdi) ++
      popq rsi ++ popq rdi ++ (* e1 dans rdi, e2 dans rsi *)
      code_op

  | TEbinop (Beq | Bne as op, e1, e2) ->
    (* TODO code pour egalite toute valeur *)
    let eq_jmp = match op with
      | Beq -> je
      | Bne -> jne
      | _ -> assert false (* arrive jamais mais warning sinon *)
    in
    expr env e1 ++ pushq (reg rdi) ++
    expr env e2 ++ pushq (reg rdi) ++
    popq rsi ++ popq rdi ++
    (match e1.expr_typ with
      | Tint | Tbool | Tptr _ -> cmpq (reg rsi) (reg rdi) ++ compile_bool eq_jmp
      | Tstring -> call "strcmp@PLT" ++ cmpl (imm 0) (reg eax) ++ compile_bool eq_jmp
      | _ -> assert false (* TODO egualité autres types *))

  | TEunop (Uneg, e1) ->
    (* TODO code pour negation ints DONE *)
    expr env e1 ++
    movq (reg rdi) (reg rsi) ++ movq (imm 0) (reg rdi) ++
    subq (reg rsi) (reg rdi)

  | TEunop (Unot, e1) ->
    (* TODO code pour negation bool DONE *)
    expr env e1 ++
    testq (reg rdi) (reg rdi) ++
    compile_bool jz

  | TEunop (Uamp, e1) ->
    (* TODO code pour & DONE *)
    l_val_addr env e1

  | TEunop (Ustar, e1) ->
    (* TODO code pour * DONE *)
    l_val_addr env e ++
    movq (ind rdi) (reg rdi)

  | TEprint el ->
    (* TODO code pour Print *) (* TEMPO !!!! -> less but still naive version*)
    let print_one = function
      | Tint -> call "print_int"
      | Tbool -> call "print_bool"
      | Tstring -> call "print_string"
      | Tptr _ -> call "print_pointer" 
      | _ -> assert false in
    (match el with
      | [] ->
          nop
      | [ex] ->
          expr env ex ++ print_one ex.expr_typ
      | ex :: ex_rest ->
          expr env ex ++ print_one ex.expr_typ ++
          call "print_space" ++
          expr env {expr_desc = TEprint ex_rest ; expr_typ = e.expr_typ})

  | TEident x ->
    (* TODO code pour x DONE *)
    movq (ind rbp ~ofs:x.v_addr) (reg rdi) ++
    movq (ind rdi) (reg rdi)

  | TEassign (lvl, el) ->
      let assign_lv code lv =
        l_val_addr env lv ++
        popq r12 ++
        movq (reg r12) (ind rdi) ++
        code
      in
      let eval_vars = proper_eval_list env el in
      let assign_all_lv = List.fold_left assign_lv nop lvl in
      eval_vars ++ assign_all_lv
(*
  | TEassign ([{expr_desc=TEident x}], [e1]) ->
    (* TODO code pour x = e *)
    expr env e1 ++
    movq (ind rbp ~ofs:x.v_addr) (reg rax) ++
    movq (reg rdi) (ind rax)

  | TEassign ([lv], [e1]) ->
    (* TODO code pour x1,... = e1,... *) assert false

  | TEassign (_, _) ->
     assert false
*)
  | TEblock el ->
     (* TODO code pour block *) (* TEMPO !!!! -> naive version *)
      List.fold_left (fun res e -> res ++ expr env e) nop el

  | TEif (e1, e2, e3) ->
     (* TODO code pour if *) assert false

  | TEfor (e1, e2) ->
     (* TODO code pour for *) assert false

  | TEnew ty ->
     (* TODO code pour new S *) assert false

  | TEcall (f, el) ->
     (* TODO code pour appel fonction *) assert false

  | TEdot (e1, {f_ofs=ofs}) ->
     (* TODO code pour e.f *) assert false

  | TEvars (vl, el) ->
     (* TODO créations de variables puis assignations *)
      let add_var code v =
        env.nb_locals := !(env.nb_locals) + 1;
        v.v_addr <- -8 * !(env.nb_locals);
        code ++
        movq (imm (sizeof v.v_typ)) (reg rdi) ++ call "allocz" ++
        movq (reg rax) (ind rbp ~ofs:v.v_addr)
      in
      let assign_var code v =
        movq (ind rbp ~ofs:v.v_addr) (reg rax) ++
        popq rdi ++
        movq (reg rdi) (ind rax) ++
        code
      in
      let add_all_vars = List.fold_left add_var nop vl in
      let eval_exprs = proper_eval_list env el in
      let assign_all_vars = List.fold_left assign_var nop vl in
      add_all_vars ++ eval_exprs ++ assign_all_vars

  | TEreturn [] ->
    (* TODO code pour return e *) assert false

  | TEreturn [e1] ->
    (* TODO code pour return e1,... *) assert false

  | TEreturn _ ->
     assert false

  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- DONE *)
    match op with
    | Inc -> incq (reg rdi)
    | Dec -> decq (reg rdi)

and l_val_addr env e = match e.expr_desc with
  | TEident x ->
      movq (ind rbp ~ofs:x.v_addr) (reg rdi)
  | TEdot _ ->
      assert false (*TODO*)
  | TEunop (Ustar, e1) ->
      l_val_addr env e1 ++
      movq (ind rdi) (reg rdi)
  | _ -> assert false (* impossible si bien typé *)

and proper_eval_list env e_lst = match e_lst with
  | [] ->
      nop
  | [{expr_desc = TEcall _ }] ->
      assert false (* TODO *)
  | _ ->
      List.fold_left (fun code e -> code ++ expr env e ++ pushq (reg rdi)) nop e_lst

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *)
  let s = f.fn_name in
  let env_fun = empty_env in
  let expr_fun = expr env_fun e in
  label ("F_" ^ s) ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  subq (imm (!(env_fun.nb_locals) * 8)) (reg rsp) ++
  expr_fun ++
  movq (reg rbp) (reg rsp) ++ 
  popq rbp ++
  ret (*TEMPO !!!!! *)
  

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

(* ----- allocation functions ----- *)

let allocz_fun =
  label "allocz" ++
  movq (reg rdi) (reg rbx) ++
  call "malloc" ++
  testq (reg rbx) (reg rbx) ++
  jnz "1f" ++
  ret ++
  label "1" ++
  movb  (imm 0) (ind rax ~index:rbx) ++
  decq (reg rbx) ++
  jnz "1b" ++
  ret

(* ----------- *)

let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  (* TODO code fonctions *) let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
      allocz_fun ++
      FmtPrint.print_functions;

   (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      FmtPrint.print_data ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }

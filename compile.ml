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

(* ----- AUXILARY FUNCTIONS ----- *)

(* ----- label gestion ----- *)

let strings = Hashtbl.create 32
let alloc_string =
  let r = ref 0 in
  fun s ->
    incr r;
    let l = "S_" ^ string_of_int !r in
    Hashtbl.add strings l s;
    l

let new_label =
  let r = ref 0 in fun () -> incr r; "L_" ^ string_of_int !r


(* ----- type gestion ----- *)

let sizeof = Typing.sizeof
let eq_type = Typing.eq_type

let rec is_struct = function
  | Tstruct _ -> true
  | Tmany [typ] -> is_struct typ
  | _ -> false

let rec is_pointer_struct = function
  | Tptr typ -> is_struct typ
  | Tmany [typ] -> is_pointer_struct typ
  | _ -> false

let type_of_lst = function
  | [{ expr_desc = TEcall (f, _) }] -> f.fn_typ
  | el -> List.map (fun ex -> ex.expr_typ) el


(* ----- code shortcut ----- *)

let malloc n = movq (imm n) (reg rdi) ++ call "malloc"
let allocz n = movq (imm n) (reg rdi) ++ call "allocz"

let set_up_div = movq (reg rdi) (reg rax) ++ cqto

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

(* ----- memory gestion code ----- *)

(* deep_copy function, warning : %rax and %rbx are used *)
(* source and destination are register with the address to copy in *)
let deep_copy_fun source dest size =
  movq (imm size) (reg rbx) ++
  label "1" ++
  subq (imm 8) (reg rbx) ++
  movq (ind source ~index:rbx) (reg rax) ++
  movq (reg rax) (ind dest ~index:rbx) ++
  testq (reg rbx) (reg rbx) ++
  jnz "1b"

let rec assign address value = function
  | Twild -> nop
  | Tstruct s -> deep_copy_fun value address (sizeof (Tstruct s))
  | Tmany [typ] -> assign address value typ
  | _ -> movq (reg value) (ind address)

(* size to alloc in bits in rdi *)
let allocz_fun =
  label "allocz" ++
  movq (reg rdi) (reg rbx) ++
  call "malloc" ++
  label "1" ++
  decq (reg rbx) ++
  movb  (imm 0) (ind rax ~index:rbx) ++
  testq (reg rbx) (reg rbx) ++
  jnz "1b" ++
  ret

(* ----- env gestion ----- *)

type env = {
  exit_label: string;
  ofs_this: int;
  nb_locals: int ref; (* maximum *)
  next_local: int; (* 0, 1, ... *)
}

let new_env exit ofs =
  { exit_label = exit ; ofs_this = ofs ; nb_locals = ref 0 ; next_local = 0 }



(* ----- CODE GENERATION ----- *)

(* ----- main code generator functions ----- *)

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
    let rec wich_eq = function
      | Tint | Tbool | Tptr _ -> cmpq (reg rsi) (reg rdi)
      | Tstring -> call "strcmp@PLT" ++ cmpl (imm 0) (reg eax)
      | Tmany [typ] -> wich_eq typ
      | _ -> assert false (* TODO egualité autres types *)
    in
    let eq_jmp = match op with
      | Beq -> je
      | Bne -> jne
      | _ -> assert false (* arrive jamais mais warning sinon *)
    in
    expr env e1 ++ pushq (reg rdi) ++
    expr env e2 ++ pushq (reg rdi) ++
    popq rsi ++ popq rdi ++
    wich_eq e1.expr_typ ++
    compile_bool eq_jmp

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
    get_addr env e1

  | TEunop (Ustar, e1) ->
    (* TODO code pour * DONE *)
    expr env e1 ++
    (if is_struct e.expr_typ then
      nop
    else
      movq (ind rdi) (reg rdi))

  | TEprint el ->
    (* TODO code pour Print DONE *)
    let rec run_printing = function
      | [] -> []
      | [typ] -> [(typ, false)]
      | typ1 :: typ2 :: typ_rest ->
          (typ1, not (eq_type typ1 Tstring || eq_type typ2 Tstring)) :: run_printing (typ2 :: typ_rest)
    in
    let print_expr (typ, print_space) =
      FmtPrint.print_one typ ++
      (if print_space then call "print_space" else nop)
    in
    efficient_eval_list env el print_expr (run_printing (type_of_lst el))

  | TEident x ->
    (* TODO code pour x DONE *)
      movq (ind rbp ~ofs:x.v_addr) (reg rdi)

  | TEassign (lvl, el) -> 
      let assign_lv code lv =
        code ++
        get_addr env lv ++
        popq rsi ++
        assign rdi rsi lv.expr_typ
      in
      let eval_vars = put_list_on_stack env el in
      let assign_all_lv = List.fold_left assign_lv nop lvl in
      eval_vars ++ assign_all_lv

  | TEblock el ->
     (* TODO code pour block *) (* TEMPO !!!! -> naive version *)
      List.fold_left (fun res e -> res ++ expr env e) nop el

  | TEif (e1, e2, e3) ->
     (* TODO code pour if DONE *)
      let end_if_lab = new_label () in
      let else_lab = new_label () in
      expr env e1 ++
      testq (reg rdi) (reg rdi) ++
      jz else_lab ++
      expr env e2 ++
      jmp end_if_lab ++
      label else_lab ++
      expr env e3 ++
      label end_if_lab

  | TEfor (e1, e2) ->
     (* TODO code pour for DONE *)
      let loop_lab = new_label () in
      let loop_end_lab = new_label () in
      label loop_lab ++
      expr env e1 ++
      testq (reg rdi) (reg rdi) ++
      jz loop_end_lab ++
      expr env e2 ++
      jmp loop_lab ++
      label loop_end_lab

  | TEnew ty ->
     (* TODO code pour new S DONE *)
      allocz (sizeof ty) ++
      movq (reg rax) (reg rdi)

  | TEcall (f, el) ->
     (* TODO code pour appel fonction *)
      let return_size = 8 * (List.length f.fn_typ) in
      let params_size = 8 * (List.length f.fn_params) in
      subq (imm return_size) (reg rsp) ++
      put_list_on_stack env el ++
      call ("F_" ^ f.fn_name) ++
      (if (List.length f.fn_typ) = 1 then
        addq (imm (params_size)) (reg rsp) ++
        popq rdi
      else
        addq (imm (params_size + return_size)) (reg rsp))
  
  | TEdot (lv, f) ->
     (* TODO code pour e.f DONE *) (* simplify with ofs(%rdi) ?*)
      get_addr env e ++
      (if is_struct f.f_typ then
        nop
      else
        movq (ind rdi) (reg rdi))

  | TEvars (vl, el) ->
     (* TODO créations de variables puis assignations *)
      let add_var v =
        if v.v_name = "_" then
          ()
        else
          env.nb_locals := !(env.nb_locals) + 1;
          v.v_addr <- -8 * !(env.nb_locals);
      in
      let var_assign v =
        if v.v_name = "_" then
          nop
        else
          (match is_struct v.v_typ, el = [] with
          | false, true ->
              movq (imm 0) (ind rbp ~ofs:v.v_addr)
          | false, false ->
              movq (reg rdi) (ind rbp ~ofs:v.v_addr)
          | true, true ->
              allocz (sizeof v.v_typ) ++
              movq (reg rax) (ind rbp ~ofs:v.v_addr)
          | true, false ->
              let struct_size = sizeof v.v_typ in
              pushq (reg rdi) ++
              malloc struct_size ++
              movq (reg rax) (reg rsi) ++
              popq rdi ++
              deep_copy_fun rdi rsi struct_size)
      in
      let complete_el =
        if el = [] then
          List.map (fun x -> {expr_desc = TEskip; expr_typ = Twild}) vl
        else 
          el
      in
      List.iter add_var vl;
      efficient_eval_list env complete_el var_assign vl 

  | TEreturn el ->
    (* TODO code pour return *)
    let eval_ret =
      let ofs_ret = ref env.ofs_this in
      fun typ ->
        ofs_ret := !ofs_ret + 8;
        movq (reg rdi) (ind rbp ~ofs:(!ofs_ret - 8))
    in
    efficient_eval_list env el eval_ret (type_of_lst el) ++
    jmp env.exit_label

  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- DONE *)
    let action = match op with
      | Inc -> incq (reg r12)
      | Dec -> decq (reg r12) in
    get_addr env e1 ++
    movq (ind rdi) (reg r12) ++
    action ++
    movq (reg r12) (ind rdi)

and get_addr env e = match e.expr_desc with
  | TEident x ->
      if x.v_name = "_" then
        nop
      else if is_struct x.v_typ then
        movq (ind rbp ~ofs:x.v_addr) (reg rdi)
      else
        movq (reg rbp) (reg rdi) ++
        addq (imm x.v_addr) (reg rdi)
  | TEdot (e1, f1) ->
      get_addr env e1 ++
      (if (is_pointer_struct e1.expr_typ) then
        movq (ind rdi) (reg rdi)
      else
        nop) ++
      addq (imm f1.f_ofs) (reg rdi)
  | TEunop (Ustar, e1) ->
        get_addr env e1 ++
        movq (ind rdi) (reg rdi)
  | _ -> assert false (*impossible si bien typé *)

and put_list_on_stack env e_lst = match e_lst with
  | [] ->
      nop
  | [{expr_desc = TEcall (f, _) } as ex] ->
      expr env ex ++
      subq (imm (8 * (List.length f.fn_typ))) (reg rsp)
  | _ ->
      List.fold_left (fun code e -> expr env e ++ pushq (reg rdi) ++ code) nop e_lst

and efficient_eval_list : 'a. env -> Tast.expr list -> ('a -> X86_64.text) -> 'a list -> X86_64.text =
  fun env e_lst exec exec_arg_lst -> match e_lst with
  | [] ->
      nop
  | [{ expr_desc = TEcall (f, _) } as ex] ->
      expr env ex ++
      subq (imm (8 * (List.length f.fn_typ))) (reg rsp) ++
      List.fold_left (fun code arg -> code ++ popq rdi ++ exec arg) nop exec_arg_lst
  | _ ->
      List.fold_left2 (fun code ex arg -> code ++ expr env ex ++ exec arg) nop e_lst exec_arg_lst



(* ----- SETUP FUNCTIONS ----- *)

(* ----- setup functions args ----- *)
let set_up_params params =
  let rec aux lst i = match lst with
    | [] -> ()
    | param ::rest ->
        param.v_addr <- 8 * i;
        aux rest (i + 1)
  in aux params 2 (* +1 for return address on the stack *)

let rec set_up_structs params =  match params with
  | param :: rest_params when is_struct param.v_typ ->
      let size_s = sizeof param.v_typ in
      pushq (ind rbp ~ofs:param.v_addr) ++
      malloc size_s ++
      movq (reg rax) (reg rdi) ++
      popq rsi ++
      deep_copy_fun rsi rdi size_s ++
      movq (reg rdi) (ind rbp ~ofs:param.v_addr)
  | _ ->
      nop

let function_ f e =
  if !debug then eprintf "function %s:@." f.fn_name;
  (* TODO code pour fonction *)
  set_up_params f.fn_params;
  let s = f.fn_name in
  let ofs_fun = 16 + 8 * (List.length f.fn_params) in
  let env_fun = new_env ("E_" ^ s) ofs_fun in
  let expr_fun = expr env_fun e in
  label ("F_" ^ s) ++
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  set_up_structs f.fn_params ++
  subq (imm (!(env_fun.nb_locals) * 8)) (reg rsp) ++
  expr_fun ++
  label env_fun.exit_label ++
  movq (reg rbp) (reg rsp) ++ 
  popq rbp ++
  ret (*TEMPO !!!!! *)


(* ----- setup offset ----- *)

let set_offset_fields fields =
  let ofs = ref 0 in
  let set_offset f =
    f.f_ofs <- !ofs;
    ofs := !ofs + sizeof(f.f_typ) in
  Hashtbl.iter (fun key f -> set_offset f) fields

let set_offset_structs = function
  | TDstruct s -> set_offset_fields s.s_fields
  | TDfunction _ -> ()



(* ----- MAIN FUNCTION ----- *)

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

let file ?debug:(b=false) dl =
  debug := b;
  (* TODO calcul offset champs *)
  List.iter set_offset_structs dl;
  (* TODO code fonctions *)
  let funs = List.fold_left decl nop dl in
  { text =
      globl "main" ++ label "main" ++
      call "F_main" ++
      xorq (reg rax) (reg rax) ++
      ret ++
      funs ++
      allocz_fun ++
      !FmtPrint.print_functions;

   (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      !FmtPrint.print_data ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }

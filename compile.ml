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
  { exit_label = "" ; ofs_this = -1 ; nb_locals = ref 0 ; next_local = 0 }

let new_env exit ofs =
  { exit_label = exit ; ofs_this = ofs ; nb_locals = ref 0 ; next_local = 0 }

let mk_bool d = { expr_desc = d; expr_typ = Tbool }
let mk_ident v = { expr_desc = TEident v; expr_typ = v.v_typ }

(* f reçoit le label correspondant à ``renvoyer vrai'' *)
let compile_bool f =
  let l_true = new_label () and l_end = new_label () in
  f l_true ++
  movq (imm 0) (reg rdi) ++ jmp l_end ++
  label l_true ++ movq (imm 1) (reg rdi) ++ label l_end

(* general case of assignation, assuming val is on the stack and direction is in rdi *)
let rec assign_lv_general = function
  | Twild -> nop
  | Tstruct s ->
      popq rsi ++
      movq (imm (sizeof (Tstruct s))) (reg rbx) ++
      call "deep_copy"
  | _ ->
      popq rsi ++
      movq (reg rsi) (ind rdi)

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
    l_val_addr env e1

  | TEunop (Ustar, e1) ->
    (* TODO code pour * DONE *)
    l_val_addr env e ++
    (match e.expr_typ with
      | Tstruct _ -> nop
      | _ -> movq (ind rdi) (reg rdi))

  | TEprint el ->
    (* TODO code pour Print DONE *)
    let rec run_printing = function
      | [] ->
          nop
      | [typ] ->
          popq rdi ++ FmtPrint.print_one ~go:true typ
      | typ1 :: typ2 :: typ_rest ->
          popq rdi ++ FmtPrint.print_one ~go:true typ1 ++
          (if typ1 = Tstring || typ2 = Tstring then nop else call "print_space") ++
          run_printing (typ2 :: typ_rest)
    in
    let type_lst = function
      | [{ expr_desc = TEcall (f, _) }] -> f.fn_typ
      | el -> List.map (fun ex -> ex.expr_typ) el
    in
    proper_eval_list env el ++
    run_printing (type_lst el)

  | TEident x ->
    (* TODO code pour x DONE *)
      movq (ind rbp ~ofs:x.v_addr) (reg rdi)

  | TEassign (lvl, el) ->
      let assign_lv code lv =
        code ++
        l_val_addr env lv ++
        assign_lv_general lv.expr_typ
      in
      let eval_vars = proper_eval_list env el in
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
      movq (imm (sizeof ty)) (reg rdi) ++
      call "allocz" ++
      (match ty with
        | Tstruct s ->
            pushq (reg rax) ++
            movq (imm 8) (reg rdi) ++
            call "allocz" ++
            popq rdi ++
            movq (reg rdi) (ind rax) ++
            movq (reg rax) (reg rdi)
        | _ ->
            movq (reg rax) (reg rdi))

  | TEcall (f, el) ->
     (* TODO code pour appel fonction *)
      let return_size = 8 * (List.length f.fn_typ) in
      let params_size = 8 * (List.length f.fn_params) in
      subq (imm return_size) (reg rsp) ++
      proper_eval_list env el ++
      call ("F_" ^ f.fn_name) ++
      (if (List.length f.fn_typ) = 1 then
        addq (imm (params_size)) (reg rsp) ++
        popq rdi
      else
        addq (imm (params_size + return_size)) (reg rsp))
  
  | TEdot (lv, f) ->
     (* TODO code pour e.f DONE *) (* simplify with ofs(%rdi) ?*)
      l_val_addr env e ++
      (match f.f_typ with
        | Tstruct _ -> nop
        | _ -> movq (ind rdi) (reg rdi))

  | TEvars (vl, el) ->
     (* TODO créations de variables puis assignations *)
      let add_var code v =
        if v.v_name = "_" then nop else (
        env.nb_locals := !(env.nb_locals) + 1;
        v.v_addr <- -8 * !(env.nb_locals);
        code ++
        (match v.v_typ with
          | Tstruct _ ->
              movq (imm (sizeof v.v_typ)) (reg rdi) ++ call "allocz" ++
              movq (reg rax) (ind rbp ~ofs:v.v_addr)
          | _ ->
              movq (imm 0) (ind rbp ~ofs:v.v_addr)))
      in
      let assign_var code v =
        code ++
        l_val_addr env (mk_ident v) ++
        assign_lv_general v.v_typ
      in
      let add_all_vars = List.fold_left add_var nop vl in
      let eval_exprs = proper_eval_list env el in
      let assign_all_vars = if el = [] then nop else List.fold_left assign_var nop vl in
      add_all_vars ++ eval_exprs ++ assign_all_vars

  | TEreturn el ->
    (* TODO code pour return *)
    let rec eval_ret e_lst ofs_ret = match e_lst with
      | [] ->
          nop
      | ex :: rest ->
          popq rdi ++
          movq (reg rdi) (ind rbp ~ofs:ofs_ret) ++
          eval_ret rest (ofs_ret + 8)
    in
    proper_eval_list env el ++
    eval_ret el env.ofs_this ++
    jmp env.exit_label

  | TEincdec (e1, op) ->
    (* TODO code pour return e++, e-- DONE *)
    let action = match op with
      | Inc -> incq (reg r12)
      | Dec -> decq (reg r12) in
    l_val_addr env e1 ++
    movq (ind rdi) (reg r12) ++
    action ++
    movq (reg r12) (ind rdi)

and l_val_addr env e = match e.expr_desc with
  | TEident x ->
      if x.v_name = "_" then nop else
      (match x.v_typ with
        | Tstruct s ->
            movq (ind rbp ~ofs:x.v_addr) (reg rdi)
        | _ ->
          movq (reg rbp) (reg rdi) ++
          addq (imm x.v_addr) (reg rdi))
  | TEdot ({ expr_typ = Tptr _ } as e1, f1) ->
      l_val_addr env e1 ++
      movq (ind rdi) (reg rdi) ++
      addq (imm f1.f_ofs) (reg rdi)
  | TEdot (e1, f1) ->
      l_val_addr env e1 ++
      addq (imm f1.f_ofs) (reg rdi)
  | TEunop (Ustar, e1) ->
      l_val_addr env e1 ++
      movq (ind rdi) (reg rdi)
  | _ -> assert false (* impossible si bien typé *)

and proper_eval_list env e_lst = match e_lst with
  | [] ->
      nop
  | [{expr_desc = TEcall (f, _) } as ex] ->
      expr env ex ++
      subq (imm (8 * (List.length f.fn_typ))) (reg rsp)
  | _ ->
      List.fold_left (fun code e -> expr env e ++ pushq (reg rdi) ++ code) nop e_lst

let set_up_params params =
  let rec aux lst i = match lst with
    | [] -> ()
    | param ::rest ->
        param.v_addr <- 8 * i;
        aux rest (i + 1)
  in aux params 2 (* +1 for return address on the stack *)

let rec set_up_structs params =  match params with
  | ({v_typ = Tstruct s} as param) :: rest_params ->
      let size_s = sizeof (Tstruct s) in
      pushq (ind rbp ~ofs:param.v_addr) ++
      movq (imm size_s) (reg rdi) ++
      call "allocz" ++
      popq rsi ++
      movq (reg rax) (reg rdi) ++
      movq (imm size_s) (reg rbx) ++
      call "deep_copy" ++
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
  

let decl code = function
  | TDfunction (f, e) -> code ++ function_ f e
  | TDstruct _ -> code

(* ----- memory gestion functions ----- *)

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

(* source address in rsi, destination address in rdi, size to copy in rbx *)
let deep_copy_fun =
  label "deep_copy" ++
  subq (imm 8) (reg rbx) ++
  movq (ind rsi ~index:rbx) (reg rax) ++
  movq (reg rax) (ind rdi ~index:rbx) ++
  testq (reg rbx) (reg rbx) ++
  jnz "deep_copy" ++
  ret

(* ----- offset computing ----- *)

let set_offset_fields fields =
  let ofs = ref 0 in
  let set_offset f =
    f.f_ofs <- !ofs;
    ofs := !ofs + sizeof(f.f_typ) in
  Hashtbl.iter (fun key f -> set_offset f) fields

let set_offset_structs = function
  | TDstruct s -> set_offset_fields s.s_fields
  | TDfunction _ -> ()

(* ---------- *)

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
      deep_copy_fun ++
      FmtPrint.print_functions;

   (* TODO print pour d'autres valeurs *)
   (* TODO appel malloc de stdlib *)
    data =
      FmtPrint.print_data ++
      (Hashtbl.fold (fun l s d -> label l ++ string s ++ d) strings nop)
    ;
  }

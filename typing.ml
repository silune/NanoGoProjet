
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

(* TODO environnement pour les types structure *)

let new_struct name fields size = {s_name = name; s_fields = fields; s_size = size}

module EnvStruct = struct
  module M = Map.Make(String)
  type t = structure M.t
  let empty = M.empty
  let find = M.find
  let add env s = M.add s.s_name s env

  let all_structs = ref []
  
  let structure name fields size =
    let s = new_struct name fields size in
    all_stucts := s :: !all_structs;
    add env s, s


(* TODO environnement pour les fonctions *)

let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | _ -> error dummy_loc ("unknown struct ") (* TODO type structure *)

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | _ -> false
    (* TODO autres types *)

(* not TODO but done ? *)
let rec is_simple_type = function
  | Tint | Tbool | Tstring | Tstruct -> true
  | Tptr ty -> is_simple_type ty
  | Tmany [ty] -> is_simple_type ty
  | _ -> false

(* not TODO but done ? *)
let rec string_of_type = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tstruct stru -> stru.s_name
  | Tptr ty -> string_of_type ty ^ " pointer"
  | Tmany [ty] -> string_of_type ty
  | _ -> "undefined type"

let wrong_type operand expected_type real_type =
        operand ^ "expression of the operator is expected of type " ^ (string_of_type requiered_type) ^
        " but has type " ^ (string_of_type real_type) in

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = 0; v_depth = 0 }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c ->
     (* TODO done ? *)
     let type_of_constant = function
       | Cbool _ -> Tbool | Cint _ -> Tint | Cstring _ -> Tstring in
     TEconstant c, type_of_constant c, false

  | PEbinop (op, e1, e2) ->
     (* TODO done ? *)
     let expr1, rt1 = expr env e1 in
     let expr2, rt2 = expr env e2 in
     let required_type, output_type = match op with
       | Beq | Bne | Blt | Ble | Bgt | Bge -> Tint, Tbool
       | Badd | Bsub | Bmul | Bdiv | Bmod -> Tint, Tint
       | Band | Bor -> Tbool, Tbool in
     if (op == Beq || op == Bne) && (expr1.expr_desc == TEnil && expr2.expr_desc == TEnil)
        then error loc "operator not defined for two nil pointer"
        else if not (equ_type expr1.expr_typ required_type)
           then error e1.pexpr_loc (wrong_type "right " required_type expr1.expr_ty)
           else if not (equ_type expr2.expr_typ required_type)
              then error e2.pexpr_loc (wrong_type "left " required_type expr2.expr_ty)
              else TEbinop (op, expr1, expr2), output_type, false

  | PEunop (Uamp, e1) ->
     (* TODO done ? *)
     let expr_desc_e1, typ_e1, rt_e1 = expr_left_val env loc e1 in
        TEunop (Uamp, expr_desc_e1, Tptr typ_e1, rt_e1

  | PEunop (Ustar, e1) ->
     epxr_left_val env loc (PEunop (Ustar, e1))

  | PEunop (Uneg | Unot as op, e1) ->
     (* TODO done ? *)
     let expr1, rt1 = expr env e1 in
     let required_type, output_typ = match op with
       | Uneg -> Tint, Tint
       | Unot -> Tbool, Tbool in
     if not (equ_type expr1.expr_typ required_type)
        then error loc (wrong_type "" required_type expr1.expr_typ)
        else TEunop (op, expr1.expr1), output_type, false

  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO done ? *)
    let try_simple_type expr_elt = if not (is_simple_type expr_elt.expr_ty)
       then error loc "fmt.Print expect simple types"
       else () in
    let expr_lst = List.map fst (List.map (expr env) el) in (match expr_lst with
       | [({expr_desc = (TEcall _, _)}, _)] -> TEprint expr_lst, tvoid, false
       | _ -> List.iter try_simple_type expr_lst; TEprint expr_lst, tvoid, false

  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty  = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       |  -> (* TODO *) error loc ("no such type " ^ id) in
     TEnew ty, Tptr ty, false

  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *) assert false
  | PEfor (e, b) ->
     (* TODO *) assert false
  | PEif (e1, e2, e3) ->
     (* TODO *) assert false
  | PEnil ->
     (* TODO *) assert false
  | PEident {id=id} ->
     (* TODO *) (try let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *) assert false
  | PEassign (lvl, el) ->
     (* TODO *) TEassign ([], []), tvoid, false 
  | PEreturn el ->
     (* TODO *) TEreturn [], tvoid, true
  | PEblock el ->
     (* TODO *) TEblock [], tvoid, false
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false

and expr_left_val env loc = function
        | PEident {id=id} ->
           try let v = Env.find id env in TEident v, v.v_typ, false
           with Not_found -> error loc ("unbound variable " ^ id)
        | Pedot (e, {loc=locX; id=idX}) ->
           let exprE, typE rtE = expr_left_val env loc in
           let exprRes, rtRes = expr env Pedot (e, {loc=locX; id=idX}) in
           exprRes.expr_desc, exprRes.expr_typ, rtRes
        | PEunop (Ustar, pexpr) ->
           let exprE, rtE = expr env expr in
           if exprE.expr_desc = TEnil
                then error loc ("nil pointer doesn't have value")
                else (match exprE.typ with
                        | Tptr typ -> exprE.expr_desc typ rtE
                        | typ -> error loc ("expression of type " ^ (string_of_type typ) ^ " but expected of type " ^ (string_of_type typ)))
        | _ -> error loc "lvalue required here"

let found_main = ref false

(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }} -> (* TODO *) ()
  | PDfunction _ -> ()

let sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | _ -> (* TODO *) assert false 

(* 2. declare functions and type fields *)
let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } ->
     (* TODO *) () 
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
     (* TODO *) () 

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *) 
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let e, rt = expr Env.empty e in
    TDfunction (f, e)
  | PDstruct {ps_name={id}} ->
    (* TODO *) let s = { s_name = id; s_fields = Hashtbl.create 5; s_size = 0 } in
     TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  (* fmt_imported := imp; *)
  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl

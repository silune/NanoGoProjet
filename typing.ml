
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

(* ---------- Environnement des types structures (Hashtbl) ---------- *)

module EnvStruct = struct

  let structEnv = Hashtbl.create 5
  let find = Hashtbl.find structEnv
  let mem = Hashtbl.mem structEnv

  (* Transforme une pstructure en structure primitive (champ vide et taille nulle) *)
  let pstruct_to_newstruct ({ps_name = {id=id; loc=loc}; ps_fields = pfl}) =
    { s_name = id; s_fields = Hashtbl.create (List.length pfl); s_size = 0 }
  
  (* Ajoute, à partir d'une pstructure, une structure primitive au contexte *)
  let add ps = Hashtbl.add structEnv ps.ps_name.id (pstruct_to_newstruct ps)

  (* Transforme un ptype en type -> verifie que le type est bien construit *)
  let rec ptype_to_type = function
    | PTident { id = typName; loc = loc} ->
        (try
          let s = find typName in
          Tstruct s
        with Not_found ->
          (match typName with
          | "int" -> Tint
          | "bool" -> Tbool
          | "string" -> Tstring
          | _ -> error loc ("type " ^ typName ^ " is not defined")))
    | PTptr pty -> Tptr (ptype_to_type pty)

  (* Parcour récursivement une liste de champs et verifie si ils sont bien typés 
    + ajoute chaques champs dans une table donnée en argument *)
  let rec check_fields id_struct fields = function
    | ({id=id; loc=loc}, ptyp)::q ->
        if Hashtbl.mem fields id then
          error loc ("structure field declaration conflict : two field with name \"" ^ id ^ "\" in " ^ id_struct);
        let typ = ptype_to_type ptyp in
        Hashtbl.add fields id { f_name = id; f_typ = typ; f_ofs = 0; f_order = (Hashtbl.length fields) };
        check_fields id_struct fields q
    | [] -> ()

  (* Remplie (avec vérification de typage) les champs d'une structure à par partir d'une liste de pfield *)
  let check_pstructure_fields id_struct ps_fields =
    let s = find id_struct in
    check_fields id_struct s.s_fields ps_fields

  (* Verifie si un champ est définit récursivement en memorisant les types rencontrés dans 'memory' *)
  let rec check_recursive memory = function
    | Tint | Tbool | Tstring | Tptr _ -> ()
    | Tstruct s ->
        if List.mem s.s_name memory then
          error dummy_loc ("the type " ^ s.s_name ^ " is ill-formed : recursive definition")
        else
          let new_memory = (s.s_name :: memory) in
          Hashtbl.iter (fun _ field -> check_recursive new_memory field.f_typ) s.s_fields
    | _ -> error dummy_loc ("field definition should be simple type")

  (* Verifie qu'une structure n'est pas récursive *)
  let check_structure_recursive id_struct = check_recursive [] (Tstruct (find id_struct))

end

(* ---------- Environnement pour les fonctions (Hashtbl) ---------- *)

module EnvFun = struct

  let funEnv = Hashtbl.create 5
  let find = Hashtbl.find funEnv
  let mem = Hashtbl.mem funEnv

  let add f =
    Hashtbl.add funEnv f.fn_name f

  (* Verifie qu'uneliste de parmaetre est bien typée et posssède des noms de variables distincts *)
  let rec check_params id_fun memory = function
    | ({id=id; loc=loc}, ptyp)::q ->
        if List.mem id memory then
          error loc ("parameters declaration conflict : two variables with name \"" ^ id ^ "\" in " ^ id_fun);
        ignore (EnvStruct.ptype_to_type ptyp);
        check_params id_fun (id::memory) q
    | [] -> ()

  let check_pfunction_params id_fun = (fun pf_params -> check_params id_fun [] pf_params)

  let check_pfunction_types = List.map EnvStruct.ptype_to_type

  (* --- Gestion d'une Hashtbl d'environnement de variables --- *)
  (* permet de stocker l'environnement de l'ensemble des variables d'une fonction *)
  let funVarEnv = Hashtbl.create 5
  let find_env f_name = Hashtbl.find funVarEnv f_name
  let add_env f_name = Hashtbl.add funVarEnv f_name

end

(* ---------- Fonctions relatifs à la gestion des types / ptypes ---------- *)

(* Transforme un ptyp en typ *)
let rec type_type = EnvStruct.ptype_to_type

(* Donne l'égalité entre deux types *)
let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring | Tmany [], Tmany [] -> true
  | Twild, _ | _, Twild -> true
  | Tstruct s1, Tstruct s2 -> s1 == s2
  | Tptr typ1, Tptr typ2 -> eq_type typ1 typ2
  | Tmany [typ1], typ2 | typ2, Tmany [typ1] -> eq_type typ1 typ2
  | Tmany (typ1::q1), Tmany (typ2::q2) -> (eq_type typ1 typ2) && (eq_type (Tmany q1) (Tmany q2))
  | _ -> false

(* Transforme un type en son nom (string) *)
let rec string_of_type = function
  | Tint -> "int"
  | Tbool -> "bool"
  | Tstring -> "string"
  | Tstruct stru -> stru.s_name
  | Tptr Twild -> "nil"
  | Tptr ty -> string_of_type ty ^ " pointer"
  | Tmany [ty] -> string_of_type ty
  | Tmany [] -> "void"
  | Tmany lst -> "(" ^ (string_of_type_lst lst) ^ ")"
  | _ -> "undefined type"

and string_of_type_lst = function
  | [] -> ""
  | [ty] -> string_of_type ty
  | ty :: q -> (string_of_type ty) ^ ", " ^ (string_of_type_lst q)

(* Retourne la liste des types d'entrée d'une fonction *)
let function_input_typ f =
   Tmany (List.map (fun v -> v.v_typ) (f.fn_params))

(* Verifie si un type est bien formé *)
let rec is_well_formed = function
  | Tint | Tbool | Tstring | Tstruct _ | Twild -> true
  | Tptr ty | Tmany [ty] -> is_well_formed ty
  | _ -> false

(* Verifie si un type est de type pointer et renvoie le type vers lequel il pointe *)
let rec pointer_type loc = function
  | Tptr typ -> typ
  | Tmany [typ] -> pointer_type loc typ
  | _ -> error loc "should be a pointer type"

(* Test si une expression est de type bien formé, renvoie une erreur si n'est pas le cas *)
let check_well_formed id loc expr_elt =
  if not (is_well_formed expr_elt.expr_typ) then
    error loc (id ^ " expect well formed simple type, " ^ (string_of_type expr_elt.expr_typ) ^ " is not")
  else ()

(* Renvoie une chaine de caractère pour une erreur de mauvais type *)
let wrong_type loc requiered_type wrong_one_type wrong_thing =
  error loc
  (wrong_thing ^ " is expected of type " ^ (string_of_type requiered_type) ^ 
   " but type " ^ (string_of_type wrong_one_type ^ " is given"))

(* Test si deux listes de types sont égales, renvoie une erreur si non *)
let check_type_2_lst id loc lst1 lst2 =
  if List.length lst1 <> List.length lst2 then
    error loc ("arity error, " ^ id ^ " expects " ^ (string_of_int (List.length lst1)) ^ " arguments")
  else
    let combined = List.combine lst1 lst2 in
    let equal = List.for_all (fun (ty1, ty2) -> eq_type ty1 ty2) combined in
    if not equal then
      wrong_type loc (Tmany lst1) (Tmany lst2) id
    else ()

(* Test si tout les types d'une liste sont d'un type donné, renvoie une erreur si non *)
let check_typ_lst loc typ lst =
  let right_type = List.for_all (fun t -> eq_type t typ) lst in
  if not right_type then
    error loc ("all expressions her are expected of type " ^ (string_of_type typ) ^ " but some are not")
  else ()


(* ---------- Initialisation informations globales sur le code ---------- *)

let fmt_used = ref false
let fmt_imported = ref false
let found_main = ref false
let depth = ref 0


(* ---------- Environnements pour les variables (Map.Make(String)) ---------- *)

(* Transforme une variable en expression *)
let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

(* Effectue la geston des nouvelles variables et de l'unicité des 'id' *)
let new_var =
  let id = ref (-1) in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = 0; v_depth = !depth }


(* --- Module pour les environnements --- *)

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let no_care = new_var "_" dummy_loc Twild
  let empty = M.add "_" no_care (M.empty)
  let find = M.find
  let mem = M.mem
  let add env v = 
    if mem v.v_name env then
      if (find v.v_name env).v_depth = !depth then
        error v.v_loc ("two variable with the same name : " ^ v.v_name);
      M.add v.v_name v env
  
  let all_vars = ref [no_care]
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (not v.v_used) then error v.v_loc "unused variable" in
    List.iter check !all_vars

  let var x loc ?used ty env =
    if x = "_" then
      env, no_care
    else
      let v = new_var x loc ?used ty in
      all_vars := v :: !all_vars;
      add env v, v

  (* fonction var mais qui exclu les variables "_", pour la declaration de fonctions *)
  let var_param x loc ?used ty env =
    if x = "_" then
      error loc "'_' is not an acceptable variable name"
    else
      var x loc ?used ty env
      
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid


(* ---------- Transformation des Pexpressions en expression ---------- *)

let ret_type_lst = ref []

(* Transforme une pexpr en expr sur l'environnement 'env' *)
let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

(* Traite récursivement chaque description de pexpr *)
and expr_desc env loc = function
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c ->
     let type_of_constant = function
       | Cbool _ -> Tbool | Cint _ -> Tint | Cstring _ -> Tstring in
     TEconstant c, type_of_constant c, false

  | PEbinop (Beq | Bne as op, e1, e2) ->
     let expr1, rt1 = expr env e1 in
     let expr2, rt2 = expr env e2 in
     if (expr1.expr_desc == TEnil && expr2.expr_desc == TEnil) then
       error loc "operator not defined for two nil pointer"
     else if not (eq_type expr1.expr_typ expr2.expr_typ) then
       error loc "this operator expects two expression with the same type"
     else TEbinop (op, expr1, expr2), Tbool, false

  | PEbinop (op, e1, e2) ->
     let expr1, rt1 = expr env e1 in
     let expr2, rt2 = expr env e2 in
     let required_type, output_type = match op with
       | Blt | Ble | Bgt | Bge -> Tint, Tbool
       | Badd | Bsub | Bmul | Bdiv | Bmod -> Tint, Tint
       | Band | Bor -> Tbool, Tbool 
       | Beq | Bne -> Twild, Tbool (* impossible mais warning sinon *) in
     if not (eq_type expr1.expr_typ required_type) then
       wrong_type e1.pexpr_loc required_type expr1.expr_typ "right expression applied to the operator"
     else if not (eq_type expr2.expr_typ required_type) then 
       wrong_type e2.pexpr_loc required_type expr2.expr_typ "left expression applied to the operator"
     else TEbinop (op, expr1, expr2), output_type, false

  | PEunop (Uamp, e1) ->
     let expr_e1, rt_e1 = l_val env e1 in
        TEunop (Uamp, expr_e1), Tptr expr_e1.expr_typ, rt_e1

  | PEunop (Ustar, e1) as ex ->
     l_val_desc env loc ex

  | PEunop (Uneg | Unot as op, e1) ->
     let expr1, rt1 = expr env e1 in
     let required_type, output_type = match op with
       | Uneg -> Tint, Tint
       | Unot -> Tbool, Tbool
       | _ -> Twild, Twild (*impossible mais warning sinon *) in
     if not (eq_type expr1.expr_typ required_type) then
       wrong_type e1.pexpr_loc required_type expr1.expr_typ "expression applied to the operator"
     else TEunop (op, expr1), output_type, false

  | PEcall ({id = "fmt.Print"}, el) ->
    if not !fmt_imported then
      error loc "you should import fmt to use fmt.Print"
    else ();
    fmt_used := true;
    let expr_lst, _ = proper_check_list_type el "fmt.Print" env in
    TEprint expr_lst, tvoid, false

  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}; pexpr_loc=pexpr_loc}]) ->
     let ty  =
       (match id with
        | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
        | _ -> try (let s = EnvStruct.find id in Tstruct s)
                with Not_found -> error pexpr_loc ("no such type " ^ id)) in
     TEnew ty, Tptr ty, false
        
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"

  | PEcall ({id=id}, el) ->
     let f =
       try (EnvFun.find id)
       with Not_found -> error loc ("no such function " ^ id) in
     let expr_lst, type_lst = proper_check_list_type el f.fn_name env in
     let input_typ_lst = List.map (fun v -> v.v_typ) f.fn_params in
     check_type_2_lst f.fn_name loc input_typ_lst type_lst;
     TEcall (f, expr_lst), Tmany f.fn_typ, false

  | PEfor (e, b) ->
     let e_expr, _ = expr env e in
     let b_expr, rtb = expr env b in
     if not (eq_type Tbool e_expr.expr_typ) then
       wrong_type e.pexpr_loc Tbool e_expr.expr_typ "'for' condition expression"
     else TEfor (e_expr, b_expr), tvoid, rtb

  | PEif (e1, e2, e3) ->
     let e1_expr, _ = expr env e1 in
     let e2_expr, rt1 = expr env e2 in
     let e3_expr, rt2 = expr env e3 in
     if not (eq_type Tbool e1_expr.expr_typ) then
       wrong_type e1.pexpr_loc Tbool e1_expr.expr_typ "'if' condition expression"
     else TEif (e1_expr, e2_expr, e3_expr), tvoid, rt1 && rt2  

  | PEnil ->
     TEnil, Tptr Twild, false

  | PEident {id=id} ->
      if id = "_" then
        error loc "'_' is not a variable"
      else
        (try let v = Env.find id env in
        v.v_used <- true;
        TEident v, v.v_typ, false
        with Not_found -> error loc ("unbound variable " ^ id))

  | PEdot (e, {loc=loc; id=id}) ->
     let e_expr, _ = expr env e in
     let e_struct = match e_expr.expr_typ with
       | Tstruct s | Tptr Tstruct s -> s
       | _ -> error e.pexpr_loc ((string_of_type e_expr.expr_typ) ^ " is not a structure type") in
     let field = (try Hashtbl.find e_struct.s_fields id
                  with Not_found -> error loc ((string_of_type e_expr.expr_typ) ^ " has no field " ^ id)) in
        TEdot (e_expr, field), field.f_typ, false

  | PEassign (lvl, el) ->
     let check_assign pexpr = (match pexpr.pexpr_desc with
       | PEident {id = "_"} -> {expr_desc = TEident (Env.no_care); expr_typ = Twild}, false
       | _ -> l_val env pexpr) in
     let lv_expr_l, _ = List.split (List.map check_assign lvl) in
     let lv_type_l = List.map (fun e -> e.expr_typ) lv_expr_l in
     let expr_l, type_l = proper_check_list_type el "assignment" env in
     check_type_2_lst "this declaration" loc lv_type_l type_l;
     TEassign (lv_expr_l, expr_l), tvoid, false

  | PEreturn el ->
      let expr_l, type_l = proper_check_list_type el "return" env in
      check_type_2_lst "return in the function" loc !ret_type_lst type_l;
      TEreturn expr_l, tvoid, true

  | PEblock el ->
     (match el with
      | [] -> TEblock [], tvoid, false
      | e::b ->
          let e_expr, rete = expr env e in
          depth := !depth + 1;
          let next_env =
            (match e_expr.expr_desc with
            | TEvars (var_l, expr_l) -> List.fold_left Env.add env var_l
            | _ -> env) in
          let b_expr, retb = expr next_env {pexpr_desc = PEblock b; pexpr_loc=loc} in
          (match b_expr.expr_desc with
            | TEblock q -> depth := !depth - 1; TEblock (e_expr::q), tvoid, retb || rete
            | _ -> TEblock [], tvoid, retb || rete (* impossible mais warning sinon *)))

  | PEincdec (e, op) ->
      let e_expr, _ = expr env e in
      if not (eq_type e_expr.expr_typ Tint) then
        wrong_type e.pexpr_loc Tint e_expr.expr_typ "expression applied to this unary operator"
     else TEincdec (e_expr, op), Tint, false

  | PEvars (lvl, typo, el) ->
      let expr_l, type_l = proper_check_list_type el "var declaration" env in
      let var_lst =
        (match (Option.is_some typo, List.length type_l) with
        | true, tlen when (tlen = List.length lvl) || (tlen = 0) ->
            let req_typ = type_type (Option.get typo) in
            check_typ_lst loc req_typ type_l;
            List.map (fun var_id -> snd (Env.var var_id.id var_id.loc req_typ env)) lvl
        | false, 0 ->
            error loc "variable declaration require at least a type or some expressions"
        | false, tlen when tlen = List.length lvl ->
            let lv_type_lst = List.combine lvl type_l in
            List.iter (fun e -> if e.expr_desc = TEnil then error loc "nil cannot be assign in declaration") expr_l;
            List.map (fun (var_id, typ) -> snd (Env.var var_id.id var_id.loc typ env)) lv_type_lst
        | _, _ ->
            error loc "arrity error : there should be as many expression as there is variable declared") in
      TEvars (var_lst, expr_l), tvoid, false

(* Renvoie expr env e mais verifie si e est une l-value dans env *)
and l_val env e =
   let e, ty, rt = l_val_desc env e.pexpr_loc e.pexpr_desc in
      { expr_desc = e; expr_typ = ty }, rt

(* Equivalent de expr_desc pour les l-values *)
and l_val_desc env loc = function
  | PEident pexpr ->
      expr_desc env loc (PEident pexpr)
  | PEdot (e, {loc=locX; id=idX}) ->
     let _, _ = l_val env e in
     expr_desc env loc (PEdot (e, {loc=locX; id=idX}))
  | PEunop (Ustar, pexpr) ->
     let exprE, rtE = expr env pexpr in
     if exprE.expr_desc = TEnil then
       error loc "*nil is not defined"
     else
       let typ = pointer_type loc exprE.expr_typ in
       TEunop (Ustar, exprE), typ, rtE
  | _ -> error loc "lvalue required here"

(* Renvoie une liste d'expression et une liste de types à partir d'une liste de pexpressions *)
  (* le but ici est de parcourir la liste en se souvenant de la position des expressions,
     de gerer le cas d'appels de fonctions,
     de verifier que tout les types sont bien formés *)
and proper_check_list_type pexpr_lst action_name env =
  let expr_lst = List.map (fun e -> fst (expr env e)) pexpr_lst in
  let loc_lst = (List.map (fun e -> e.pexpr_loc) pexpr_lst) in
  let expr_loc_lst = List.combine expr_lst loc_lst in
  let type_lst =
    (match expr_loc_lst with
    | [{expr_desc = TEcall (g, _)}, g_loc] ->
        let res =
          if List.length g.fn_typ = 0 then [tvoid] else g.fn_typ in
        List.iter
          (fun ty ->
            if not (is_well_formed ty) then
              error g_loc ("well formed types are expected, " ^ (string_of_type ty) ^ " is not"))
          res;
        res
    | _ ->
        List.map (fun (e, e_loc) -> check_well_formed action_name e_loc e; e.expr_typ) expr_loc_lst) in
  expr_lst, type_lst


(* ---------- Phases de declarations des structures et des fonctions ---------- *)

(* 1. declare structures *)
let phase1 = function
  | PDstruct ({ ps_name = { id = id; loc = loc }} as ps) ->
      if EnvStruct.mem id
        then error loc ("structure declaration conflict : two structures with name \"" ^ id ^ "\"")
        else EnvStruct.add ps
  | PDfunction _ -> ()

let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tstruct s -> Hashtbl.fold (fun _ field sum -> sum + (sizeof field.f_typ)) s.s_fields 0
  | Tmany lst -> List.fold_left (fun sum typ -> sum + (sizeof typ)) 0 lst
  | Twild -> assert false

(* Transform un parametre en une variable et l'ajoute à un environnemnt *)
let rec params_to_vars env = function
  | ({id=id; loc=loc}, ptyp) :: q -> 
      let new_env, v = Env.var_param id loc (type_type ptyp) env in
      let res_env, res_var_lst = params_to_vars new_env q in
      res_env, (v :: res_var_lst)
  | [] -> env, []

(* 2. declare functions and type fields *)
let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; } ->
      if id = "main" then (
        if pl <> [] then
          error loc "main function does not take any parameters";
        if tyl <> [] then
          error loc "main function does not return anything";
        found_main := true
        );

      if EnvFun.mem id then
        error loc ("function declaration conflict : two functions with name \"" ^ id ^ "\"")
      else begin
        EnvFun.check_pfunction_params id pl;
        let typ_lst = EnvFun.check_pfunction_types tyl in
        let fun_env, var_lst = params_to_vars Env.empty pl in
        EnvFun.add_env id fun_env;
        let f = { fn_name = id; fn_params = var_lst; fn_typ = typ_lst} in
        EnvFun.add f 
      end;
  | PDstruct { ps_name = {id}; ps_fields = fl } ->
      EnvStruct.check_pstructure_fields id fl

(* verification des structures recursives et ajout de sizeof *)
let phase2rec = function
  | PDstruct { ps_name = { id = id; loc = loc }} ->
      EnvStruct.check_structure_recursive id;
      let s = EnvStruct.find id in
      s.s_size <- sizeof (Tstruct s)
  | PDfunction _ -> ()

(* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    let env = EnvFun.find_env id in
    let f = EnvFun.find id in
    ret_type_lst := f.fn_typ;
    let body_expr, rt = expr env e in
    if (not rt) && (List.length tyl > 0) then
      error loc ("function " ^ id ^ " may not return")
    else TDfunction (f, body_expr)
  | PDstruct {ps_name={id}} ->
    let s = EnvStruct.find id in
    TDstruct s


(* ---------- Fonction principale ---------- *)

let file ~debug:b (imp, dl) =
  debug := b;
  fmt_imported := imp;
  List.iter phase1 dl;
  List.iter phase2 dl;
  List.iter phase2rec dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused ();
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl

open Ast

(* Exceptions *)

exception Typing_error of string

let raise_error params = raise (Typing_error (match params with   
    | "typ_nexi"::a::[] -> Printf.sprintf "Le type '%s' n'existe pas." a
    | "typ_ndef"::a::[] -> Printf.sprintf "Le type '%s' n'est pas défini." a
    | "typ_nmat"::a::b::[] -> Printf.sprintf "L'expression est de type '%s' mais devrait être de type '%s'." a b
    | "typf_nexi"::a::b::[] -> Printf.sprintf "Le type '%s' n'a pas de champs '%s'." a b
    | "var_ronly"::[] -> Printf.sprintf "La variable n'est pas modifiable."
    | "var_nexi"::a::[] -> Printf.sprintf "La variable '%s' n'existe pas." a
    | "pro_nexi"::a::[] -> Printf.sprintf "La procédure '%s' n'existe pas." a
    | "fun_nexi"::a::[] -> Printf.sprintf "La fonction '%s' n'existe pas." a
    | "fun_nhave_return"::[] -> Printf.sprintf "La fonction ne finit pas toujours avec une instruction 'return'."
    | "ide_already_taken"::a::b::[] -> Printf.sprintf "'%s' est déjà un nom de %s." a b
    | "ide_bad_kind"::a::b::[] -> Printf.sprintf "'%s' correspond à un nom de %s." a b
    | "not_left_val"::[] -> Printf.sprintf "Une valeur gauche est attendue."
    | "not_fov"::a::[] -> Printf.sprintf "'%s' n'est pas une fonction ou une variable." a
    | "not_unique"::a::[] -> Printf.sprintf "L'entrée '%s' est déjà utilisé." a
    | "bad_nb_params"::a::b::[] -> Printf.sprintf "%s paramètre(s) ont été reçus mais %s sont attendus." a b
    | a::q -> a
    | [] -> ""
))

(* Environnement *)

(*
La variable 'env' est une liste de key * def ordonnée par :
    - nom croissant
    - profondeur décroissante
*)

type key = string * int

(* typpr : type précis (utilise une clé et non une simple chaîne) *)
type typpr = TPRrec of key | TPRaccOFrec of key | Ttypenull

type deftyp = DefTrec of (string * typpr) list | DefTaccOFrec of key | DefTundefined | DefTprimitive
type def = DefT of deftyp | DefV of (pmode * typpr) | DefP of (pmode * typpr) list | DefF of ((pmode * typpr) list * typpr)

let rec env_get env (ident1, depth1) kind1 = match env with
    | ((ident2, depth2), record2)::q ->
        if (depth1 = -1 && ident1 = ident2)  || (depth1 = depth2 && ident1 = ident2) then (
            if kind1 <> "" then (match (kind1, record2) with
                | ("typ", DefT _) -> ((ident2, depth2), record2)
                | ("typ", _) -> raise_error ["ide_bad_kind"; ident1; "type"]
                | ("var", DefV _) -> ((ident2, depth2), record2)
                | ("var", _) -> raise_error ["ide_bad_kind"; ident1; "variable"]
                | ("pro", DefP _) -> ((ident2, depth2), record2)
                | ("pro", _) -> raise_error ["ide_bad_kind"; ident1; "procédure"]
                | ("fun", DefF _) -> ((ident2, depth2), record2)
                | ("fun", _) -> raise_error ["ide_bad_kind"; ident1; "fonction"]
                | _ -> assert false)
            else ((ident2, depth2), record2)
        )
        else env_get q (ident1, depth1) kind1
    | [] -> raise_error [kind1^"_nexi"; ident1]

let env_get_key env key kind = fst(env_get env key kind)
let env_get_def env key kind = snd(env_get env key kind)

let get_type_def env key = match env_get_def env key "typ" with | DefT(c) -> c | _ -> assert false
let rec get_type_size env typpr = match typpr with
    | TPRrec(key) -> (match get_type_def env key with
        | DefTrec(l) -> List.fold_left (fun s (i, t) -> s + (get_type_size env t)) 0 l
        | DefTaccOFrec(key) -> 8
        | DefTprimitive -> 8
        | _ -> assert false)
    | TPRaccOFrec(key) -> 8
    | _ -> assert false
(*
Renvoie :
    - nb_follow : le nombre de pointeur à suivre
    - offset : la position dans la mémoire
    - typpr : le type du champs
*)
let rec get_type_field_type env key1 ident2 = match get_type_def env key1 with
    | DefTrec(l) ->
        let (offset1, typpr1) = (let rec aux offset = (function
            | [] -> raise_error ["typf_nexi"; fst key1; ident2]
            | (i, t)::q when i = ident2 -> (offset, t)
            | (i, t)::q -> aux (offset - get_type_size env t) q
        ) in aux 0 l) in
        (0, offset1, typpr1)
    | DefTaccOFrec(key1) ->
        let (nb_follow1, offset1, typpr1) = get_type_field_type env key1 ident2 in
        (nb_follow1 + 1, offset1, typpr1)
    | _ -> raise_error ["typf_nexi"; fst key1; ident2]
let get_var_def env ident = match env_get_def env (ident, -1) "var" with | DefV(c) -> c | _ -> assert false
let get_proc_def env ident = match env_get_def env (ident, -1) "pro" with | DefP(c) -> c | _ -> assert false
let get_func_def env ident = match env_get_def env (ident, -1) "fun" with | DefF(c) -> c | _ -> assert false

(*
On peut ajouter :
    - une variable, une procédure, une fonction et un type indéfini si le nom n'est pas déjà pris
    - un type défini si le nom n'est pas déjà pris par tout sauf un type indéfini
*)
let rec env_add env (ident1, depth1) record1 = match env with
    | ((ident2, depth2), record2)::q -> (match (String.compare ident1 ident2, depth1 - depth2) with
        | (1, _) | (2, _) -> ((ident2, depth2), record2)::(env_add q (ident1, depth1) record1)
        | (0, d) when d < 0 -> assert false
        | (0, 0) -> (match (record1, record2) with
            | (DefT(DefTrec _), DefT(DefTundefined)) -> ((ident1, depth1), record1)::q
            | _ -> (match record2 with
                | DefT _ -> raise_error ["ide_already_taken"; ident1; "type"]
                | DefV _ -> raise_error ["ide_already_taken"; ident1; "variable"]
                | DefP _ -> raise_error ["ide_already_taken"; ident1; "procédure"]
                | DefF _ -> raise_error ["ide_already_taken"; ident1; "fonction"]))
        | _ -> ((ident1, depth1), record1)::env)
    | [] -> [((ident1, depth1), record1)]
let add_type env key def = env_add env key (DefT def)
let add_var env key def = env_add env key (DefV def)
let add_proc env key def = env_add env key (DefP def)
let add_func env key def = env_add env key (DefF def)

(* Position dans le fichier *)

let pos = ref (Lexing.dummy_pos, Lexing.dummy_pos)

(* Corps du typer *)

let program fichier =
    (* Helper pour les convertions *)

    let rec string_of_typpr typ = match typ with
        | TPRrec(key) -> (match key with
            | (ident, 0) -> Printf.sprintf "%s primitif" ident
            | _ -> Printf.sprintf "%s" (fst key))
        | TPRaccOFrec(key) -> Printf.sprintf "access %s" (string_of_typpr (TPRrec key))
        | Ttypenull -> "null"
    in
    (*
    b = 
        | true      si le type doit être défini
        | false     si le type doit juste exister
    *)
    let typpr_of_typna env typ b = match typ with
        | TNAaccOFrec(ident) ->
            (try TPRaccOFrec(env_get_key env (ident, -1) "typ")
            with Typing_error _ -> raise_error ["typ_nexi"; string_of_typpr (TPRaccOFrec(ident, 1))])
        | TNArec(ident) ->
            (try (
                let (key, def) = env_get env (ident, -1) "typ" in
                if b then (match def with
                    | DefT DefTundefined -> raise_error ["typ_ndef"; string_of_typpr (TPRrec(ident, 1))]
                    | _ -> TPRrec(key)
                ) else TPRrec(key)
            )
            with Typing_error _ -> raise_error ["typ_nexi"; string_of_typpr (TPRrec (ident, 1))])
        | Ttypenull -> Ttypenull
    in

    (* Helper pour la vérification *)

    let check_if_types_are_equal env t1l t2l =
        let rec check_equality_from_key env key1 key2 =
            if key1 = key2 then true
            else match (get_type_def env key1, get_type_def env key2) with
                | (DefTaccOFrec(ident3), DefTaccOFrec(ident4)) -> check_equality_from_key env ident3 ident4
                | _ -> false
        in
        let check_equality_from_type typs t1 t2 = match (t1, t2) with
            | (Ttypenull, TPRrec(key1)) | (TPRrec(key1), Ttypenull) -> (match get_type_def env key1 with
                | DefTaccOFrec(_) -> true
                | _ -> false)
            | (Ttypenull, TPRaccOFrec(_)) | (TPRaccOFrec(_), Ttypenull) ->
                true
            | (TPRaccOFrec(key1), TPRaccOFrec(key2)) | (TPRrec(key1), TPRrec(key2)) ->
                check_equality_from_key env key1 key2
            | (TPRaccOFrec(key1), TPRrec(key2)) | (TPRrec(key2), TPRaccOFrec(key1)) -> (match get_type_def env key2 with
                | DefTaccOFrec(key3) -> check_equality_from_key env key1 key3
                | _ -> false)
            | _ ->
                t1 = t2
        in
        List.iter2 (fun t1 t2 -> 
            if not(check_equality_from_type env t1 t2) then raise_error ["typ_nmat"; string_of_typpr t1; string_of_typpr t2]
        ) t1l t2l
    in
    let check_if_all_types_are_defined env =
        List.iter (fun (key, def) -> match def with
            | DefT(DefTundefined) -> raise_error ["typ_ndef"; fst key]
            | _ -> ()
        ) env
    in
    let check_if_good_nb_params nb1 nb2 =
        if nb1 <> nb2 then raise_error ["bad_nb_params"; string_of_int nb1; string_of_int nb2];
    in
    let check_if_duplications il =
        let rec aux = function
            | a::b::q -> if a = b then raise_error ["not_unique"; a] else aux (b::q)
            | _ -> ()
        in aux (List.sort compare il)
    in
    let rec check_if_instr_has_return instr =
        let rec aux instr = pos := instr.pi_pos; match instr.pi_desc with
            | PIblock(instrl) -> List.exists (fun i -> aux i) instrl
            | PIassign(_, _) | PIprocedure(_, _) | PIwhile(_, _) | PIfor(_, _, _, _, _) -> false
            | PIif(eil) -> List.for_all (fun (e, i) -> aux i) eil
            | PIreturn(_) -> true
        in
        if not(aux instr) then raise_error ["fun_nhave_return"] 
    in

    (* Fonctions du typeur *)

    (*
    Renvoie :
        - (mode, typpr) : un couple contenant le mode et le type de la valeur gauche
        - tree : le nouvel arbre
    *)
    let rec type_left_value env expr = (pos := expr.pe_pos; match expr.pe_desc with
        | PEacces(PAident(ident1)) ->
            get_var_def env ident1, Eacces(Aident(ident1))
        | PEacces(PAfield(expr1, ident2)) ->
            let type1, tree1 = type_expr env expr1 in
            let mode, key, add_follow = (match type1 with
                | TPRrec(key1) -> (match get_type_def env key1 with
                    | DefTaccOFrec(key2) ->
                        (PMinout, key2, 1)
                    | DefTrec(_) -> 
                        (fst(fst(type_left_value env expr1)), key1, 0)
                    | _ -> assert false)
                | TPRaccOFrec(key1) ->
                    (PMinout, key1, 1)
                | _ -> raise_error ["not_left_val"]
            ) in
            let (nb_follow2, offset2, type2) = get_type_field_type env key ident2 in
            (mode, type2), Eacces(Afield(tree1, (nb_follow2+add_follow, offset2, get_type_size env type2)))
        | _ -> raise_error ["not_left_val"])
    (*
    Renvoie :
        - typpr : le type de l'expression
        - tree : le nouvel arbre
    *)
    and type_expr env expr = pos := expr.pe_pos; match expr.pe_desc with
        | PEnull -> Ttypenull, Enull
        | PEint(i) -> TPRrec("integer", 0), Eint(i)
        | PEchar(c) -> TPRrec("character", 0), Echar(c)
        | PEtrue -> TPRrec("boolean", 0), Etrue
        | PEfalse -> TPRrec("boolean", 0), Efalse
        | PEacces(PAident(ident1)) ->
            (match env_get_def env (ident1, -1) "" with
                | DefT _ | DefP _ ->
                    raise_error ["not_fov"; ident1]
                | DefV _ ->
                    snd (get_var_def env ident1),
                    Eacces(Aident(ident1))
                | DefF _ ->
                    fst (type_expr env { pe_desc = PEfunction(ident1, []); pe_pos = expr.pe_pos }),
                    Efunction(ident1, []))
        | PEacces(PAfield(expr1, ident2)) ->
            let (typpr1, tree1) = type_expr env expr1 in
            let (nb_follow, offset, typpr) = (match typpr1 with
                | TPRrec(key1) | TPRaccOFrec(key1) -> get_type_field_type env key1 ident2
                | _ as t -> raise_error ["typf_nexi"; string_of_typpr t; ident2]
            ) in
            typpr, Eacces(Afield(tree1, (nb_follow, offset, get_type_size env typpr)))
        | PEbinop(expr1, op, expr2) ->
            let (type1, tree1) = type_expr env expr1 in
            let (type2, tree2) = type_expr env expr2 in
            (match op with
                | Bplus | Bminus | Bmul | Bdiv | Brem ->
                    check_if_types_are_equal env [type1; type2] [TPRrec("integer", 0); TPRrec("integer", 0)];
                    TPRrec("integer", 0)
                | Bequal | Bnequal ->
                    check_if_types_are_equal env [type2] [type1];
                    TPRrec("boolean", 0)
                | Bless | Bleq | Bgreater | Bgeq ->
                    check_if_types_are_equal env [type1; type2] [TPRrec("integer", 0); TPRrec("integer", 0)];
                    TPRrec("boolean", 0)
                | Band | Bandthen | Bor | Borelse ->
                    check_if_types_are_equal env [type1; type2] [TPRrec("boolean", 0); TPRrec("boolean", 0)];
                    TPRrec("boolean", 0)
            ), Ebinop(tree1, op, tree2)
        | PEnot(expr1) ->
            let (type1, tree1) = type_expr env expr1 in
            check_if_types_are_equal env [type1] [TPRrec("boolean", 0)];
            TPRrec("boolean", 0), Enot(tree1)
        | PEfunction(ident1, exprl) ->
            let (mtl, type1) = get_func_def env ident1 in
            check_if_good_nb_params (List.length exprl) (List.length mtl);
            let treel = List.map2 (fun (mode1, type1) e -> match mode1 with
                | PMinout ->
                    let (mode2, type2), tree2 = type_left_value env e in
                    check_if_types_are_equal env [type2] [type1];
                    if mode2 = PMin then raise_error ["var_ronly"];
                    Eaddr(tree2)
                | _ ->
                    let type2, tree2 = type_expr env e in
                    check_if_types_are_equal env [type2] [type1];
                    tree2
            ) mtl exprl in
            type1, Efunction(ident1, treel)
        | PEcharval(expr1) ->
            let (type1, tree1) = type_expr env expr1 in
            check_if_types_are_equal env [type1] [TPRrec("integer", 0)];
            TPRrec("character", 0), Echarval(tree1)
        | PEnew(ident1) ->
            let key1 = env_get_key env (ident1, -1) "typ" in
            TPRaccOFrec(key1), Enew(get_type_size env (TPRrec key1))
    in

    (*
    Renvoie :
        - tree : le nouvel arbre
    *)
    let rec type_instr depth return env instr = pos := instr.pi_pos; match instr.pi_desc with
        | PIassign(acces1, expr2) ->
            let (mode1, type1), tree1 = type_left_value env { pe_desc = PEacces(acces1); pe_pos = !pos } in
            if mode1 = PMin then raise_error ["var_ronly"];
            let type2, tree2 = type_expr env expr2 in
            check_if_types_are_equal env [type2] [type1];
            (match tree1 with Eacces(acces1) -> Iassign(acces1, tree2) | _ -> assert false)
        | PIif(eil) ->
            Iif(List.map (fun (expr1, instr) ->
                let (type1, tree1) = type_expr env expr1 in
                check_if_types_are_equal env [type1] [TPRrec("boolean", 0)];
                tree1, type_instr depth return env instr;
            ) eil)
        | PIwhile(expr1, instr) ->
            let (type1, tree1) = type_expr env expr1 in
            check_if_types_are_equal env [type1] [TPRrec("boolean", 0)];
            Iwhile(tree1, type_instr depth return env instr)
        | PIfor(ident1, bool1, expr1, expr2, instr) ->
            let (type1, tree1) = type_expr env expr1 in
            let (type2, tree2) = type_expr env expr2 in
            check_if_types_are_equal env [type1; type2] [TPRrec("integer", 0); TPRrec("integer", 0)];
            Ifor(
                ident1, bool1, tree1, tree2,
                type_instr (depth+1) return (add_var env (ident1, depth+1) (PMin, TPRrec("integer", 0))) instr
            )
        | PIprocedure(ident1, exprl) ->
            let mtl = get_proc_def env ident1 in
            check_if_good_nb_params (List.length exprl) (List.length mtl);
            let treel = List.map2 (fun (mode1, type1) e -> match mode1 with
                | PMinout ->
                    let (mode2, type2), tree2 = type_left_value env e in
                    check_if_types_are_equal env [type2] [type1];
                    if mode2 = PMin then raise_error ["var_ronly"];
                    Eaddr(tree2)
                | _ ->
                    let type2, tree2 = type_expr env e in
                    check_if_types_are_equal env [type2] [type1];
                    tree2
            ) mtl exprl in
            Iprocedure(ident1, treel)
        | PIreturn(Some(expr1)) ->
            let (type1, tree1) = type_expr env expr1 in
            check_if_types_are_equal env [type1] [return];
            Ireturn(Some(tree1))
        | PIreturn(None) ->
            check_if_types_are_equal env [Ttypenull] [return];
            Ireturn(None)
        | PIblock(instrl) ->
            Iblock(List.map (fun i -> type_instr depth return env i) instrl)
    in

    (*
    Renvoie :
        - env : le nouvel environnement
        - tree : le nouvel arbre
    *)
    let rec type_decl depth env decll =
        List.fold_left (fun (env, decllex) decl -> pos := decl.pd_pos;
            let (env, declex) = (match decl.pd_desc with
            | PDrow(identl, type1, expro) ->
                let type1 = typpr_of_typna env type1 true in
                let treeo = (match expro with
                    | Some(expr1) ->
                        let (type1, tree1) = type_expr env expr1 in
                        check_if_types_are_equal env [type1] [type1];
                        Some(tree1)
                    | None ->
                        None
                ) in

                List.fold_left (fun env i -> add_var env (i, depth) (PMinout, type1)) env identl,
                Dvar(identl, get_type_size env type1, treeo)
            | PDident(ident1) ->
                add_type env (ident1, depth) DefTundefined,
                Dtype
            | PDaccess(ident1, ident2) ->
                add_type env (ident1, depth) (match typpr_of_typna env (TNArec ident2) false with
                    | TPRrec key -> DefTaccOFrec key
                    | _ -> assert false
                ), Dtype
            | PDrecord(ident1, champsl) ->
                let champsl = List.fold_left (fun itl (il, t) ->
                    let env = (try (let _ = env_get env (ident1, -1) "typ" in env)
                    with Typing_error _ -> add_type env (ident1, depth) DefTundefined) in
                    let t = typpr_of_typna env t true in
                    List.fold_left (fun itl i -> (i, t)::itl) itl il
                ) [] champsl in
                check_if_duplications (List.map (fun (i, t) -> i) champsl);

                add_type env (ident1, depth) (DefTrec champsl),
                Dtype
            | PDprocedure(ident1, paraml, decll, instr) ->
                (* Partie commune à procédure et fonction *)
                check_if_all_types_are_defined env;
                let paraml = List.map (fun (i, (m, t)) -> (i, (m, typpr_of_typna env t true))) paraml in
                let (identl, mtl) = List.split paraml in
                check_if_duplications identl;

                let env = add_proc env (ident1, depth) mtl in
                let env2 = List.fold_left2 (fun env i (m, t) -> add_var env (i, depth + 1) (m, t)) env identl mtl in
                let (env2, decll_tree) = type_decl (depth + 1) env2 decll in
                check_if_all_types_are_defined env2;
                let instr_tree = type_instr (depth+1) Ttypenull env2 instr in

                env,
                Dpf((ident1, depth),
                    List.map (fun (i, (m, t)) -> (i, m = PMinout, get_type_size env t)) paraml,
                decll_tree, instr_tree)
            | PDfunction(ident1, paraml, type1, decll, instr) ->
                (* Partie commune à procédure et fonction *)
                check_if_all_types_are_defined env;
                let paraml = List.map (fun (i, (m, t)) -> (i, (m, typpr_of_typna env t true))) paraml in
                let (identl, mtl) = List.split paraml in
                check_if_duplications identl;

                let type1 = typpr_of_typna env type1 true in
                let env = add_func env (ident1, depth) (mtl, type1) in
                let env2 = List.fold_left2 (fun env i (m, t) -> add_var env (i, depth + 1) (m, t)) env identl mtl in
                let (env2, decll_tree) = type_decl (depth + 1) env2 decll in
                check_if_all_types_are_defined env2;
                check_if_instr_has_return instr;
                let instr_tree = type_instr (depth+1) type1 env2 instr in

                env,
                Dpf((ident1, depth),
                    (".return", false, get_type_size env type1)::(List.map (fun (i, (m, t)) -> (i, m = PMinout, get_type_size env t)) paraml),
                decll_tree, instr_tree)
            ) in (env, if declex <> Dtype then decllex@[declex] else decllex)
        ) (env, []) decll
    in

    List.hd(snd(type_decl 0 (
        [("boolean", 0), DefT DefTprimitive;
        ("character", 0), DefT DefTprimitive;
        ("integer", 0), DefT DefTprimitive;
        ("put", 0), DefP [(PMin, TPRrec(("character", 0)))];
        ("new_line", 0), DefP []]
    ) [fichier]))
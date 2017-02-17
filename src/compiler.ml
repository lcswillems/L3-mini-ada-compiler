open X86_64
open Ast

(*
Convention pour les registres :
- rsp : adresse du dernier élément de la pile
- rbp : adresse de la valeur du 'rbp' appelant

Tableau d'activation :
    e1
    ..
    eN
    %rbp père
    -------------
    adr. retour
    -------------
    %rbp appelant   <- %rbp
    v1
    ..
    vN

*)

let compteur = ref 0

module Smap = Map.Make(String)

(*
env = ( 
    vars : ident -> depth, offset, by_ref, var_alloc_size
    pfs : ident -> dirty_ident, depth, return_alloc_size
)
*)

let get_var (vars, pfs) key = Smap.find key vars
let get_pf (vars, pfs) key = Smap.find key pfs

let add_var (vars, pfs) key def = (Smap.add key def vars, pfs)
let add_pf (vars, pfs) key def = (vars, Smap.add key def pfs)

let program fichier =
    (* Helper pour répéter du code nb fois *)

    let rec iter code = function 0 -> nop | n -> code ++ (iter code (n-1)) in
    let rec iter_with_acc size code =
        let rec aux = function
            | acc when acc = size -> nop
            | acc -> code acc ++ aux (acc + 8)
        in aux 0
    in

    (* Helper pour obtenir l'adresse d'une variable *)

    (* Met l'adresse de la variable dans 'rax' *)
    let rec load_var_addr depth env = function
        | Eacces(Aident(ident1)) ->
            let (depth1, offset1, by_ref1, var_alloc_size1) = get_var env ident1 in
            movq (reg rbp) (reg rax) ++
            iter (movq (ind ~ofs:16 rax) (reg rax)) (depth - depth1) ++
            addq (imm offset1) (reg rax) ++
            (if by_ref1 then movq (ind rax) (reg rax) else nop)
        | Eacces(Afield(expr2, (nb_follow2, offset2, var_alloc_size2))) ->
            load_var_addr depth env expr2 ++
            iter (movq (ind rax) (reg rax)) nb_follow2 ++
            addq (imm offset2) (reg rax)
        | _ -> assert false
    in

    (* Helper pour l'exécution d'une procédure/fonction *)

    let rec execute_pf depth1 env ident exprl =
        let dirty_ident, depth2, return_alloc_size = get_pf env ident in
        let code, size = List.fold_left (fun (wcode, wsize) expr ->
            let code, size = compile_expr depth1 env expr in
            (wcode ++ code, wsize + size)
        ) (nop, 0) exprl in
        (subq (imm return_alloc_size) (reg rsp) ++
        code ++
        movq (reg rbp) (reg rax) ++
        iter (movq (ind ~ofs:16 rax) (reg rax)) (depth1 - depth2) ++
        pushq (reg rax) ++
        call dirty_ident ++
        addq (imm (8+size)) (reg rsp)),
        return_alloc_size
    
    (* Fonctions du compilateur *)

    (* Met la valeur de l'expression sur la pile et renvoie sa taille *)
    and compile_expr depth env expr = compteur := !compteur + 1; match expr with
        | Eint(i) ->
            movq (imm i) (reg rax) ++ pushq (reg rax), 8
        | Echar(c) ->
            pushq (imm (Char.code c)), 8
        | Etrue ->
            pushq (imm 1), 8
        | Efalse ->
            pushq (imm 0), 8
        | Enull ->
            pushq (imm 0), 8
        | Eaddr(expr1) ->
            load_var_addr depth env expr1 ++
            pushq (reg rax),
            8
        | Eacces(Aident(ident1)) ->
            let (depth1, offset1, by_ref1, var_alloc_size1) = get_var env ident1 in
            load_var_addr depth env (Eacces(Aident(ident1))) ++
            iter_with_acc var_alloc_size1 (let f acc =
                pushq (ind ~ofs:(-acc) rax) in f),
            var_alloc_size1
        | Eacces(Afield(expr1, (nb_follow, offset, var_alloc_size))) ->
            let (code1, size1) = compile_expr depth env expr1 in
            code1 ++
            leaq (ind ~ofs:(size1-8) rsp) rax ++
            iter (movq (ind rax) (reg rax)) nb_follow ++
            (if nb_follow > 0 then (    
                subq (imm (var_alloc_size-size1)) (reg rsp) ++
                iter_with_acc var_alloc_size (let f acc =
                    movq (ind ~ofs:(offset-acc) rax) (reg rbx) ++
                    movq (reg rbx) (ind ~ofs:(var_alloc_size-8-acc) rsp) in f)
            ) else (
                iter_with_acc var_alloc_size (let f acc =
                    movq (ind ~ofs:(offset-acc) rax) (reg rbx) ++
                    movq (reg rbx) (ind ~ofs:(size1-8-acc) rsp) in f) ++
                addq (imm (size1-var_alloc_size)) (reg rsp)
            )),
            var_alloc_size
        | Ebinop(expr1, op, expr2) -> (match op with
            | Bnequal ->
                compile_expr depth env (Enot(Ebinop(expr1, Bequal, expr2)))
            | Bgreater ->
                compile_expr depth env (Ebinop(expr2, Bless, expr1))
            | Bgeq ->
                compile_expr depth env (Ebinop(expr2, Bleq, expr1))
            | _ ->
                let (code1, size1) = compile_expr depth env expr1 in
                let (code2, size2) = compile_expr depth env expr2 in
                (match op with
                    | (Bandthen | Borelse) as op ->
                        let cs = string_of_int !compteur in
                        let (f, b) = (match op with
                            | Bandthen -> (je, 0)
                            | Borelse -> (jne, 1)
                            | _ -> assert false
                        ) in
                        code1 ++
                        popq rax ++
                        testq (reg rax) (reg rax) ++
                        f ("e1"^"_"^cs) ++
                        code2 ++
                        jmp ("e0"^"_"^cs) ++
                        label ("e1"^"_"^cs) ++
                        pushq (imm b) ++
                        label ("e0"^"_"^cs)
                    | _ ->
                        code1 ++ code2 ++ (match op with
                        | (Bplus | Bminus | Bmul) as op ->
                            let f = (match op with
                                | Bplus -> addq
                                | Bminus -> subq
                                | Bmul -> imulq
                                | _ -> assert false
                            ) in
                            popq rbx ++
                            popq rax ++
                            f (reg rbx) (reg rax) ++
                            pushq (reg rax)
                        | Bdiv ->
                            popq rbx ++
                            popq rax ++
                            movq (imm 0) (reg rdx) ++
                            idivq (reg rbx) ++
                            pushq (reg rax)
                        | Brem ->
                            let cs = string_of_int !compteur in
                            popq rbx ++
                            popq rax ++

                            xorq (reg rcx) (reg rcx) ++ (* Compare le dividende à 0 *)
                            cmpq (reg rcx) (reg rax) ++
                            jl ("e1"^"_"^cs) ++ (* Met 1 dans 'rdi' si le dividende est positif, -1 sinon *)
                            movq (imm 1) (reg rdi) ++
                            jmp ("e0"^"_"^cs) ++
                            label ("e1"^"_"^cs) ++
                            movq (imm (-1)) (reg rdi) ++
                            label ("e0"^"_"^cs) ++

                            imulq (reg rdi) (reg rax) ++
                            movq (imm 0) (reg rdx) ++
                            idivq (reg rbx) ++
                            imulq (reg rdi) (reg rdx) ++
                            pushq (reg rdx)
                        | Bequal ->
                            let cs = string_of_int !compteur in
                            iter_with_acc size1 (let f acc = 
                                movq (ind ~ofs:(2*size1-acc-8) rsp) (reg rbx) ++
                                movq (ind ~ofs:(size1-acc-8) rsp) (reg rax) ++
                                cmpq (reg rbx) (reg rax) ++
                                jne ("f"^"_"^cs) in f) ++
                            addq (imm (size1+size2)) (reg rsp) ++
                            pushq (imm 1) ++
                            jmp ("e"^"_"^cs) ++
                            label ("f"^"_"^cs) ++
                            addq (imm (size1+size2)) (reg rsp) ++
                            pushq (imm 0) ++
                            label ("e"^"_"^cs)
                        | (Bless | Bleq) as op ->
                            let cs = string_of_int !compteur in
                            let f = (match op with
                                | Bless -> jl
                                | Bleq -> jle
                                | _ -> assert false
                            ) in
                            popq rbx ++
                            popq rax ++
                            cmpq (reg rbx) (reg rax) ++
                            f ("e1"^"_"^cs) ++
                            pushq (imm 0) ++
                            jmp ("e0"^"_"^cs) ++
                            label ("e1"^"_"^cs) ++
                            pushq (imm 1) ++
                            label ("e0"^"_"^cs)
                        | (Band | Bor) as op ->
                            let f = (match op with
                                | Band -> andq
                                | Bor -> orq
                                | _ -> assert false
                            ) in
                            popq rbx ++
                            popq rax ++
                            f (reg rbx) (reg rax) ++
                            pushq (reg rax)
                        | _ -> assert false)
                ), 8
            )
        | Enot(expr1) ->
            fst(compile_expr depth env expr1) ++
            popq rbx ++
            movq (imm 1) (reg rax) ++
            subq (reg rbx) (reg rax) ++
            pushq (reg rax), 8
        | Enew(size) ->
            movq (imm size) (reg rdi) ++
            call "malloc@PLT" ++
            addq (imm (size-8)) (reg rax) ++
            pushq (reg rax), 8
        | Efunction(ident1, exprl) ->
            execute_pf depth env ident1 exprl
        | Echarval(expr1) ->
            compile_expr depth env expr1
    in

    let rec compile_instr depth env alloc_size instr = compteur := !compteur + 1; match instr with
        | Iassign(acces1, expr2) ->
            let (code2, size2) = compile_expr depth env expr2 in
            code2 ++
            load_var_addr depth env (Eacces acces1) ++
            iter_with_acc size2 (let f acc =
                movq (ind ~ofs:(size2-acc-8) rsp) (reg rbx) ++
                movq (reg rbx) (ind ~ofs:(-acc) rax) in f) ++
            addq (imm size2) (reg rsp)
        | Iif(eil) ->
            let cs = string_of_int !compteur in
            snd(List.fold_left (fun (i, wcode) (expr1, instr1) ->
                i-1,
                wcode ++
                label ("e"^(string_of_int i)^"_"^cs) ++
                fst(compile_expr depth env expr1) ++
                popq rax ++
                testq (reg rax) (reg rax) ++
                je ("e"^(string_of_int (i-1))^"_"^cs) ++
                compile_instr depth env alloc_size instr1 ++
                jmp ("e0"^"_"^cs)
            ) (List.length eil, nop) eil) ++
            label ("e0"^"_"^cs)
        | Iwhile(expr1, instr) ->
            let cs = string_of_int !compteur in
            jmp ("e0"^"_"^cs) ++
            label ("e1"^"_"^cs) ++
            compile_instr depth env alloc_size instr ++
            label ("e0"^"_"^cs) ++
            fst(compile_expr depth env expr1) ++
            popq rax ++
            testq (reg rax) (reg rax) ++
            jne ("e1"^"_"^cs)
        | Ifor(ident1, bool1, expr1, expr2, instr) ->
            let env = add_var env ident1 (depth, -alloc_size-8, false, 8) in
            let env = add_var env ("#"^ident1) (depth, -alloc_size-16, false, 8) in
            subq (imm 16) (reg rsp) ++
            compile_instr depth env (alloc_size+16) (Iblock([
                Iassign(Aident(ident1), if bool1 then expr2 else expr1);
                Iassign(Aident("#"^ident1), if bool1 then expr1 else expr2);
                Iwhile(
                    Ebinop(Eacces(Aident(ident1)), (if bool1 then Bgeq else Bleq), Eacces(Aident("#"^ident1))),
                    Iblock([
                        instr;
                        Iassign(
                            Aident(ident1),
                            Ebinop(Eacces(Aident(ident1)), (if bool1 then Bminus else Bplus), Eint(1))
                        )
                    ])
                )
            ])) ++
            addq (imm 16) (reg rsp)
        | Iprocedure(ident1, exprl) ->
            fst(execute_pf depth env ident1 exprl)
        | Ireturn(Some(expr1)) ->
            compile_instr depth env alloc_size (Iblock([Iassign(Aident(".return"), expr1); Ireturn(None)]))
        | Ireturn(None) ->
            movq (reg rbp) (reg rsp) ++
            popq rbp ++
            ret
        | Iblock(instrl) ->
            List.fold_left (fun code instr -> code ++ compile_instr depth env alloc_size instr) nop instrl
    in

    let rec compile_decl env decl = compteur := !compteur + 1; match decl with
        | Dpf((ident, depth), paraml, decll, instr) ->
            (* Renommage de la procédure *)
            let dirty_ident = "pf"^"_"^(string_of_int !compteur) in

            (* Ajout des paramètres dans l'environnement *)
            let (env2, next) = List.fold_right (fun (i, by_ref, var_alloc_size) (env, next) ->
                let next = next + (if by_ref then 8 else var_alloc_size) in
                (add_var env i (depth + 1, next, by_ref, var_alloc_size), next)
            ) paraml (env, 16) in

            (* Ajout de la fonction/procédure dans l'environnement *)
            let return_alloc_size = match paraml with
                | (".return", _, var_alloc_size)::q -> var_alloc_size
                | _ -> 0
            in
            let env = add_pf env ident (dirty_ident, depth, return_alloc_size) in
            let env2 = add_pf env2 ident (dirty_ident, depth, return_alloc_size) in

            (* Ajout des variables, fonctions/procédures locales dans l'environnement *)
            let (env2, code, assign, alloc_size) = List.fold_left (fun (env, wcode, assign, next) decl -> match decl with
                | Dvar(identl, var_alloc_size, expro) ->
                    List.fold_left (fun (env, wcode, assign, next) i ->
                        (add_var env i (depth + 1, -next-8, false, var_alloc_size), wcode,
                            (match expro with
                                | Some(expr) -> assign@[Iassign(Aident(i), expr)]
                            | _ -> assign),
                        next+var_alloc_size)
                    ) (env, wcode, assign, next) identl
                | _ ->
                    let (env, code) = compile_decl env decl in
                    (env, wcode ++ code, assign, next)
            ) (env2, nop, [], 0) decll in

            (* Ajout des assignements des variables dans 'instr' *)
            let instr = Iblock(assign@[instr; Ireturn(None)]) in

            (env, 
                code ++
                label dirty_ident ++
                pushq (reg rbp) ++ (* empile le rbp de l'appelant ici pour se rappeler de sa valeur *)
                movq (reg rsp) (reg rbp) ++
                subq (imm alloc_size) (reg rsp) ++
                compile_instr (depth + 1) env2 alloc_size instr                
            )
        | _ -> assert false
    in

    {
        text =
            glabel "main" ++
            pushq (reg rbp) ++
            movq (reg rsp) (reg rbp) ++
            pushq (reg rbp) ++
            call ("pf_" ^ (string_of_int 1)) ++
            popq rbp ++
            movq (imm 0) (reg rax) ++
            ret ++

            (* Déclaration de la procédure 'put' *)
            label "put" ++
            pushq (reg rbp) ++
            movq (reg rsp) (reg rbp) ++
            movq (ind ~ofs:24 rbp) (reg rdi) ++
            call "putchar@PLT" ++ 
            popq rbp ++
            movq (imm 0) (reg rax) ++
            ret ++

            (* Déclaration de la procédure 'new_line' *)
            label "new_line" ++
            pushq (reg rbp) ++
            movq (reg rsp) (reg rbp) ++
            movq (imm 10) (reg rdi) ++
            call "putchar@PLT" ++ 
            popq rbp ++
            movq (imm 0) (reg rax) ++
            ret ++

            (let (_, code) = compile_decl (Smap.empty,
                Smap.add "put" ("put", 0, 0) (Smap.add "new_line" ("new_line", 0, 0) Smap.empty)) fichier in code);
        data = nop
    }
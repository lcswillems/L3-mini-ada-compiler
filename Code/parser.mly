%{
    open Ast

    let expand_params params =
        List.fold_left (fun params (il, m, t) ->
            List.fold_left (fun params i ->
                params@[(i, (m, t))]
            ) params il
        ) [] params
%}

%token EOF
%token <int>CINT
%token <char>CCHAR
%token <string>IDENT
%token ACCESS AND BEGIN ELSE ELSIF END FALSE FOR FUNCTION IF IN IS LOOP NEW NOT NULL OR OUT PROCEDURE RECORD REM RETURN REVERSE THEN TRUE TYPE USE WHILE WITH
%token SCOLON DOT DDOT COLON LPAREN RPAREN CEQUAL APOS COMMA
%token EQUAL NEQUAL LESS LEQ GREATER GEQ PLUS MINUS MUL DIV

%left OR or_else
%left AND and_then
%nonassoc NOT
%nonassoc EQUAL NEQUAL
%nonassoc GREATER GEQ LESS LEQ
%left PLUS MINUS
%left MUL DIV REM
%nonassoc unary_minus
%left DOT

%start fichier

%type <Ast.pdecl> fichier

%%

fichier:
    WITH adatextio SCOLON USE adatextio SCOLON
    PROCEDURE ident1=IDENT IS decll=decl* 
    BEGIN instrl=instr+ END idento=IDENT? SCOLON EOF
    { if (match idento with | Some(s) -> ident1 <> s | _ -> false) then raise Parsing.Parse_error else
        { pd_desc = PDprocedure(ident1, [], decll, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }); pd_pos = ($startpos, $endpos) } }

adatextio:
    ident1=IDENT DOT ident2=IDENT { if ident1 <> "ada" || ident2 <> "text_io" then raise Parsing.Parse_error }

decl:
    desc1=decl_desc { { pd_desc = desc1; pd_pos = ($startpos, $endpos) } }
decl_desc:
    TYPE ident1=IDENT SCOLON { PDident(ident1) }
    | TYPE ident1=IDENT IS ACCESS ident2=IDENT SCOLON { PDaccess(ident1, ident2) }
    | TYPE ident1=IDENT IS RECORD champsl=champs+ END RECORD SCOLON { PDrecord(ident1, champsl) }
    | identl=separated_nonempty_list(COMMA, IDENT) COLON type1=typ expr1=assign? SCOLON { PDrow(identl, type1, expr1) }
    | PROCEDURE ident1=IDENT paramso=params? IS decll=decl* BEGIN instrl=instr+ END idento=IDENT? SCOLON
    { if (match idento with | Some(s) -> ident1 <> s | _ -> false) then raise Parsing.Parse_error else PDprocedure(ident1, (match paramso with | Some(l) -> expand_params l | _ -> []), decll, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }) }
    | FUNCTION ident1=IDENT paramso=params? RETURN type1=typ IS decll=decl* BEGIN instrl=instr+ END idento=IDENT? SCOLON
    { if (match idento with | Some(s) -> ident1 <> s | _ -> false) then raise Parsing.Parse_error else PDfunction(ident1, (match paramso with | Some(l) -> expand_params l | _ -> []), type1, decll, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }) }

assign:
    CEQUAL expr1=expr { expr1 }

champs:
    identl=separated_nonempty_list(COMMA, IDENT) COLON type1=typ SCOLON
    { (identl, type1) }

typ:
    ident1=IDENT
    { match ident1 with
        | "null" -> Ttypenull
        | _ -> TNArec(ident1) }
    | ACCESS ident1=IDENT { TNAaccOFrec(ident1) }

params:
    LPAREN paraml=separated_nonempty_list(SCOLON, param) RPAREN { paraml }

param:
    identl=separated_nonempty_list(COMMA, IDENT) COLON modeo=mode? type1=typ { (identl, (match modeo with | Some(m) -> m | _ -> PMin), type1) }

mode:
    IN { PMin }
    | IN OUT { PMinout }

expr:
    desc1=expr_desc { { pe_desc = desc1; pe_pos = ($startpos, $endpos) } }
expr_desc:
    i=CINT { PEint(i) }
    | c = CCHAR { PEchar(c) }
    | TRUE { PEtrue }
    | FALSE { PEfalse }
    | NULL { PEnull }
    | LPAREN expr1=expr RPAREN { expr1.pe_desc }
    | access1=acces { PEacces(access1) }
    | expr1=expr EQUAL expr2=expr { PEbinop(expr1, Bequal, expr2) }
    | expr1=expr NEQUAL expr2=expr { PEbinop(expr1, Bnequal, expr2) }
    | expr1=expr LESS expr2=expr { PEbinop(expr1, Bless, expr2) }
    | expr1=expr LEQ expr2=expr { PEbinop(expr1, Bleq, expr2) }
    | expr1=expr GREATER expr2=expr { PEbinop(expr1, Bgreater, expr2) }
    | expr1=expr GEQ expr2=expr { PEbinop(expr1, Bgeq, expr2) }
    | expr1=expr PLUS expr2=expr { PEbinop(expr1, Bplus, expr2) }
    | expr1=expr MINUS expr2=expr { PEbinop(expr1, Bminus, expr2) }
    | expr1=expr MUL expr2=expr { PEbinop(expr1, Bmul, expr2) }
    | expr1=expr DIV expr2=expr { PEbinop(expr1, Bdiv, expr2) }
    | expr1=expr REM expr2=expr { PEbinop(expr1, Brem, expr2) }
    | expr1=expr AND expr2=expr { PEbinop(expr1, Band, expr2) }
    | expr1=expr AND THEN expr2=expr %prec and_then { PEbinop(expr1, Bandthen, expr2) }
    | expr1=expr OR expr2=expr { PEbinop(expr1, Bor, expr2) }
    | expr1=expr OR ELSE expr2=expr %prec or_else { PEbinop(expr1, Borelse, expr2) }
    | NOT expr1=expr { PEnot(expr1) }
    | MINUS expr1=expr %prec unary_minus { PEbinop({ pe_desc = PEint(0); pe_pos = ($startpos, $endpos) }, Bminus, expr1) }
    | NEW ident1=IDENT { PEnew(ident1) }
    | ident1=IDENT LPAREN exprl=separated_nonempty_list(COMMA, expr) RPAREN { PEfunction(ident1, exprl) }
    | charval LPAREN expr1=expr RPAREN { PEcharval(expr1) }

charval:
    ident1=IDENT APOS ident2=IDENT { if ident1 <> "character" || ident2 <> "val" then raise Parsing.Parse_error }

instr:
    desc1=instr_desc { { pi_desc = desc1; pi_pos = ($startpos, $endpos) } }
instr_desc:
    access1=acces CEQUAL expr1=expr SCOLON { PIassign(access1, expr1) }
    | ident1=IDENT SCOLON { PIprocedure(ident1, []) }
    | ident1=IDENT LPAREN exprl=separated_nonempty_list(COMMA, expr) RPAREN SCOLON { PIprocedure(ident1, exprl) }
    | RETURN expro=expr? SCOLON { PIreturn(expro) }
    | BEGIN instrl=instr+ END SCOLON { PIblock(instrl) }
    | IF expr1=expr THEN instrl=instr+ elsifl=elsif* elseo=els? END IF SCOLON
    { PIif(((expr1, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) })::elsifl)@(match elseo with
        | Some(q) -> [q]
        | _ -> [({ pe_desc = PEtrue; pe_pos = ($startpos, $endpos) }, { pi_desc = PIblock([]); pi_pos = ($startpos, $endpos) })])
    ) }
    | FOR ident1=IDENT IN reverseo=REVERSE? expr1=expr DDOT expr2=expr LOOP instrl=instr+ END LOOP SCOLON
    { PIfor(ident1, (match reverseo with | Some(i) -> true | _ -> false), expr1, expr2, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }) }
    | WHILE expr1=expr LOOP instrl=instr+ END LOOP SCOLON
    { PIwhile(expr1, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }) }

elsif:
    ELSIF expr1=expr THEN instrl=instr+ { (expr1, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }) }

els:
    ELSE instrl=instr+ { ({ pe_desc = PEtrue; pe_pos = ($startpos, $endpos) }, { pi_desc = PIblock(instrl); pi_pos = ($startpos, $endpos) }) }

acces:
    ident1=IDENT { PAident(ident1) }
    | expr1=expr DOT ident1=IDENT { PAfield(expr1, ident1) }
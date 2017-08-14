type binop = Bequal | Bnequal | Bless | Bleq | Bgreater | Bgeq | Bplus | Bminus | Bmul | Bdiv | Brem | Band | Bandthen | Bor | Borelse

(* Syntaxe abstraite issue de l'analyse syntaxique *)

type pexpr = {
	pe_desc: pexpr_desc; pe_pos: Lexing.position * Lexing.position }
and pexpr_desc =
	PEint of int
	| PEchar of char
	| PEtrue
	| PEfalse
	| PEnull
	| PEacces of pacces
	| PEbinop of pexpr * binop * pexpr
	| PEnot of pexpr
	| PEnew of string
	| PEfunction of string * pexpr list
	| PEcharval of pexpr

and pacces =
	PAident of string
	| PAfield of pexpr * string

type pinstr = {
	pi_desc: pinstr_desc; pi_pos: Lexing.position * Lexing.position }
and pinstr_desc =
	PIassign of pacces * pexpr
	| PIif of (pexpr * pinstr) list
	| PIwhile of pexpr * pinstr
	| PIfor of string * bool * pexpr * pexpr * pinstr
	| PIprocedure of string * pexpr list
	| PIreturn of pexpr option
	| PIblock of pinstr list

(* typna : type naïf (utilise une simple chaîne) *)
type typna = TNArec of string | TNAaccOFrec of string | Ttypenull
type pmode = PMin | PMinout
type pparams = (string * (pmode * typna)) list
type pchamps = string list * typna

type pdecl = {
	pd_desc: pdecl_desc; pd_pos: Lexing.position * Lexing.position }
and pdecl_desc =
	PDident of string
	| PDaccess of string * string
	| PDrecord of string * pchamps list
	| PDrow of string list * typna * pexpr option
	| PDprocedure of string * pparams * pdecl list * pinstr
	| PDfunction of string * pparams * typna * pdecl list * pinstr

(* Syntaxe abstraite issue du typeur *)

type expr =
	Eint of int
	| Echar of char
	| Etrue
	| Efalse
	| Enull
	| Eaddr of expr (* pour le passage par paramètre comme dans le cours 8 *)
	| Eacces of acces
	| Ebinop of expr * binop * expr
	| Enot of expr
	| Enew of int (* 'int' correspond à la taille du type associé *)
	| Efunction of string * expr list
	| Echarval of expr

and acces =
	Aident of string
	| Afield of expr * (int * int * int)
	(* 'int * int * int' correspond :
		- au nombre de pointeur à suivre
		- à la position du champs dans le type
		- à la taille du champs *)

type instr =
	Iassign of acces * expr
	| Iif of (expr * instr) list
	| Iwhile of expr * instr
	| Ifor of string * bool * expr * expr * instr
	| Iprocedure of string * expr list
	| Ireturn of expr option
	| Iblock of instr list

type decl = 
	Dvar of string list * int * expr option
	| Dpf of (string * int) * (string * bool * int) list * decl list * instr
	| Dtype (* N'apparait pas dans l'arbre *)
open Lexing;;

try (
	let parse_only = ref false and type_only = ref false and file = ref "" in

	let get_pos pos = (pos.pos_lnum, pos.pos_cnum - pos.pos_bol + 1) in
	let print_pos (pos1, pos2) =
		let pos1 = get_pos pos1 and pos2 = get_pos pos2 in
		Printf.printf "File \"%s\", line %d, characters %d-%d:\n" !file (fst pos1) (snd pos1) (snd pos2)
	in

	Arg.parse
		["--parse-only", Arg.Set parse_only, "  Only lexing and parsing the file";
		 "--type-only", Arg.Set type_only, "  Only type checking the file"]
		(fun s -> print_string s; print_newline();
			if s = "" then (Printf.printf "No file to compile\n"; exit 1)
			else if not (Filename.check_suffix s ".adb") then (Printf.printf "The file must have .adb extension\n"; exit 1)
			else file := s
		)
		"";

	let lexbuf = Lexing.from_channel (open_in !file) in
	try
		let pass1 = Parser.fichier Lexer.token lexbuf in
		if not(!parse_only) then (
			let pass2 = Typer.program pass1 in
			if not(!type_only) then (
				let out = open_out ((Filename.chop_suffix !file ".adb") ^ ".s") in
				let fmt = Format.formatter_of_out_channel out in
				X86_64.print_program fmt (Compiler.program pass2);
				Format.fprintf fmt "@?";
				close_out out;
			)
		);
		exit 0
	with
		| Lexer.Lexing_error c ->
			print_pos (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
			Printf.printf "lexical error: %s\n" c;
			exit 1
		| Parsing.Parse_error | Parser.Error ->
			print_pos (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);
			Printf.printf "syntax error\n";
			exit 1
		| Typer.Typing_error c ->
			print_pos !(Typer.pos);
			Printf.printf "typing error : %s\n" c;
			exit 1
) with _ ->
	Printf.printf "compiler error\n";
	exit 2
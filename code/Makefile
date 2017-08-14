CMO=lexer.cmo parser.cmo typer.cmo x86_64.cmo compiler.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli
BIN=adac
FLAGS=

all: $(BIN)

clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.automaton parser.conflicts .depend ../tests/exec/*.s ../tests/exec/*.bin ../tests/exec/*.out-mien

tests: $(BIN)
	for f in ../tests/exec/*.adb; do \
		./$(BIN) $$f --type-only; \
	done
	for f in ../tests/exec-fail/*.adb; do \
		./$(BIN) $$f --type-only; \
	done
	for f in ../tests/typing/good/*.adb; do \
		./$(BIN) $$f --type-only; \
	done
	for f in ../tests/typing/bad/*.adb; do \
		./$(BIN) $$f --type-only; \
	done
	for f in ../tests/syntax/good/*.adb; do \
		./$(BIN) $$f --type-only; \
	done
	for f in ../tests/syntax/bad/*.adb; do \
		./$(BIN) $$f --type-only; \
	done
	for f in ../tests/exec/*.adb; do \
		./$(BIN) $${f%.adb}.adb; \
		gcc $${f%.adb}.s -o $${f%.adb}.bin; \
		./$${f%.adb}.bin > $${f%.adb}.out-mien; \
		diff $${f%.adb}.out $${f%.adb}.out-mien; \
	done
	for f in ../tests/exec-fail/*.adb; do \
		./$(BIN) $${f%.adb}.adb; \
		gcc $${f%.adb}.s -o $${f%.adb}.bin; \
		./$${f%.adb}.bin > $${f%.adb}.out-mien; \
		diff $${f%.adb}.out $${f%.adb}.out-mien; \
	done

$(BIN): $(CMO)
	ocamlc $(FLAGS) -o $(BIN) $(CMO)

%.cmi: %.mli
	ocamlc $(FLAGS) -c  $<

%.cmo: %.ml
	ocamlc $(FLAGS) -c  $<

%.ml: %.mll
	ocamllex $<

%.ml: %.mly
	menhir --infer -v $<

parser.ml: ast.cmi

.depend depend: $(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend
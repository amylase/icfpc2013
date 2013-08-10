.DEFAULT_GOAL = enumerate
.PHONY: all clean

SOURCE = myBatteries.ml myStd.ml syntax.ml parser.ml lexer.ml protocol.ml

OCAML_DIR = $(shell ocamlc -where)
CAMLP4 = "camlp4o pa_comprehension.cma"

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly
	$(RM) parser.mli

enumerate: $(SOURCE) enumerate.ml
	ocamlfind ocamlopt -g -pp $(CAMLP4) -package batteries,uint -linkpkg -o enumerate $(SOURCE) enumerate.ml

clean:
	$(RM) enumerate *.cmo *.cmi *.mli lexer.ml parser.ml *.o *.cmx

.DEFAULT_GOAL = main
.PHONY: all clean

SOURCE = myStd.ml syntax.ml protocol.ml

CAMLP4 = "camlp4o pa_comprehension.cma"

main: $(SOURCE) main.ml
	ocamlfind ocamlopt -g -pp $(CAMLP4) -package batteries,uint -linkpkg -o main $(SOURCE) main.ml

clean:
	$(RM) enumerate *.cmo *.cmi *.mli lexer.ml parser.ml *.o *.cmx

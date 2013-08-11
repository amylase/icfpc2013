.DEFAULT_GOAL = interp
.PHONY: all clean

SOURCE = myStd.ml syntax.ml parser.ml lexer.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

parser.ml: parser.mly
	ocamlyacc parser.mly
	$(RM) parser.mli

interp: $(SOURCE) main.ml
	ocamlfind ocamlopt -g -package batteries,uint -linkpkg -o interp $(SOURCE) main.ml

clean:
	$(RM) enumerate *.cmo *.cmi *.mli lexer.ml parser.ml *.o *.cmx

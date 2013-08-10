.DEFAULT_GOAL = main
.PHONY: all clean qtest

SOURCE = myStd.ml syntax.ml protocol.ml

CAMLP4 = "camlp4o pa_comprehension.cma"

main: $(SOURCE) main.ml
	ocamlfind ocamlopt -g -pp $(CAMLP4) -package batteries,uint -linkpkg -o main $(SOURCE) main.ml

qtest:
	mkdir -p qtest
	qtest -o qtest/all_tests.ml --shuffle extract $(SOURCE)
	ocamlfind ocamlc -pp $(CAMLP4) -package batteries,uint,oUnit,QTest2Lib -linkpkg -o qtest/all_tests $(SOURCE) qtest/all_tests.ml
	./qtest/all_tests

clean:
	$(RM) enumerate *.cmo *.cmi *.mli lexer.ml parser.ml *.o *.cmx

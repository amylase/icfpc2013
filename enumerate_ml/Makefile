.PHONY: all clean

SOURCE = myBatteries.ml myStd.ml protocol.ml

OCAML_DIR = $(shell ocamlc -where)
CAMLP4 = "camlp4o pa_comprehension.cma"

enumerate: $(SOURCE) enumerate.ml
	ocamlfind ocamlopt -g -pp $(CAMLP4) -package batteries -linkpkg -o enumerate $(SOURCE) enumerate.ml

clean:
	$(RM) enumerate *.cmo *.cmi *.mli

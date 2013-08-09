.PHONY: all clean

SOURCE = myStd.ml

PA_COMPREHENSION_PATH = ~/.opam/4.00.1/lib/pa_comprehension/pa_comprehension.cma
OCAML_DIR = $(shell ocamlc -where)
CAMLP4 = "camlp4o $(PA_COMPREHENSION_PATH)"

enumerate: $(SOURCE) enumerate.ml
	ocamlfind ocamlopt -g -pp $(CAMLP4) -package batteries -linkpkg -o enumerate $(SOURCE) enumerate.ml

clean:
	$(RM) enumerate *.cmo *.cmi *.mli

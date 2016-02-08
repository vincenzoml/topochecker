
.PHONY: all clean allclean

all: bmp_to_topochecker

%: %.native
	cp -L $^ $@

%.native: %.ml
	ocamlbuild -use-ocamlfind -pkg camlimages,camlimages.all_formats $@

clean:
	rm -rf _build *~ *.native *.topochecker bmp_to_topochecker eval.csv kripke.dot space.dot


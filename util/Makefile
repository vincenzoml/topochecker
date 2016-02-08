
.PHONY: all clean allclean

all: bmp_to_topochecker

%: %.native
	cp -L $^ $@

%.native: %.ml
	ocamlbuild -use-ocamlfind -pkg camlimages,camlimages.all_formats $@

clean:
	rm -rf _build *~ *.native

allclean: clean
	rm -rf bmp_to_topochecker

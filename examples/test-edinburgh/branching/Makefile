
run: mkmodel output
	./mkmodel

mli: mkmodel.ml
	ocamlopt.opt -i str.cmxa graphics.cmxa mkmodel.ml

mkmodel: mkmodel.ml
	ocamlopt.opt str.cmxa graphics.cmxa mkmodel.ml -o mkmodel	

%: %.bz2
	bunzip2 -k $<

output: preprocess.sh input/* input/Stops-UK.csv
	./preprocess.sh

clean:
	rm -f *~ \#* mkmodel *.cmi *.cmx *.o mkmodel.mli input/Stops-UK.csv
	rm -rf output


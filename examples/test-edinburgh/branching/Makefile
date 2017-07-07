
run: mkmodel output
	./mkmodel

mli: mkmodel.ml
	ocamlopt.opt -i str.cmxa graphics.cmxa mkmodel.ml

mkmodel: mkmodel.ml
	ocamlopt.opt str.cmxa graphics.cmxa mkmodel.ml -o mkmodel	

%: %.bz2
	bunzip2 -fk $<
#	touch $@

output: preprocess.sh input/Stops-UK.csv
	./preprocess.sh

clean:
	rm -f *~ \#* mkmodel *.cmi *.cmx *.o mkmodel.mli
	rm -rf output


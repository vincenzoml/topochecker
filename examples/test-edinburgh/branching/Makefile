OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg camlimages,camlimages.all_formats -pkg str

mkmodel: mkmodel.ml
	$(OCAMLBUILD) mkmodel.native && cp -af _build/mkmodel.native mkmodel

model/edinburgh.dot: mkmodel output/stops.csv 
	@mkdir -p model
	@mkdir -p mc_output
	@./mkmodel

model: model/edinburgh.dot

mc_output/state_0.bmp: model/edinburgh.dot experiment.topochecker
	../../../src/topochecker experiment.topochecker

view: mc_output/state_0.bmp
	xdg-open mc_output/state_0.bmp

viewdot: mc_output/state_0.bmp
	cd mc_output && dot -Tx11 ../model/edinburgh.dot

mli: mkmodel.ml
	$(OCAMLBUILD) mkmodel.inferred.mli && cat _build/mkmodel.inferred.mli

%: %.bz2
	bunzip2 -fk $<

output/stops.csv: preprocess.sh input/Stops-UK.csv
	@./preprocess.sh

clean:
	rm -f *~ \#* mkmodel *.cmi *.cmx *.o mkmodel.mli mkmodel.native
	rm -rf output
	rm -rf _build

allclean: clean
	rm -rf model
	rm -rf mc_output


SCALE=120x90

.PHONY: run clean bmp view all 

all: run

view: run
	neato -Tx11 output-0.dot

run: maketool generate_model.native
	./generate_model.native green layers/00_ground.bmp black layers/01_walls.bmp red layers/02_firstaid.bmp brown layers/03_obstacles.bmp danger layers/04_danger.bmp blue layers/05_agents_blue.bmp cyan layers/06_agents_cyan.bmp purple layers/07_agents_purple.bmp yellow layers/08_agents_yellow.bmp
	../../src/topochecker test.topochecker

%.native: *.ml
	ocamlbuild -use-ocamlfind -pkg camlimages,camlimages.all_formats $@

clean:
	rm -rf _build *~ generate_model.native generate_model output-*

bmp:
	cd layers && for f in png/*.png ; do convert $$f -background white -alpha remove -type truecolor -scale $(SCALE) $$(basename $$f .png).bmp;done


maketool:
	cd ../../src && make

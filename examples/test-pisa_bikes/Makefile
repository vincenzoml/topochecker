.PHONY: clean run maketool img
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	$(TOPOCHECKER) test.topochecker

img:
	for f in output/*-0.dot; do neato -Tpdf $$f -o output/$$(basename $$f .dot).pdf; done

maketool:
	cd ../../src && make

clean: 
	rm -rf output/* *~



.PHONY: clean run maketool img
TOPOCHECKER=../../src/topochecker

run: maketool
	mkdir -p output
	$(TOPOCHECKER) experiment1/test1.topochecker
	$(TOPOCHECKER) experiment2/test2.topochecker

img: run
	for f in output/eventuallyCluster-0.dot output/evolvingCluster-77.dot output/nextCluster-80.dot output/tail-0.dot; do neato -Tpdf $$f -o output/$$(basename $$f .dot).pdf; done

maketool:
	cd ../../src && make

clean: 
	rm -rf output/* *~



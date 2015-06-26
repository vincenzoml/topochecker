.PHONY: all clean

EXAMPLES=$(shell ls test-* -d)

all:
	for f in $(EXAMPLES); do (cd $$f && make); done

clean:
	for f in $(EXAMPLES); do (cd $$f && make clean); done
	rm -f *~

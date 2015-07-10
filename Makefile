.PHONY: all clean

src/topochecker:
	cd src && make

fast:
	cd src && make fast

all:
	cd src && make clean && make
	cd examples && make clean && make

clean:
	cd src && make clean

allclean: clean
	cd examples && make clean

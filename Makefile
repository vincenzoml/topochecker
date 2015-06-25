.PHONY: all clean

src/topochecker:
	cd src && make

all:
	cd src && make clean && make
	cd examples && make clean && make

clean:
	cd src && make clean
	cd examples && make clean

all: make-square_grid make-square_grid_no_generation

clean: clean-square_grid clean-square_grid_no_generation

clean-%: %
	echo $<
	cd $^ && make clean
	rm -f *~


make-%: %
	cd $^ && make


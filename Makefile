COMPILER_TAGS = -g -fcheck=all -Wall
LIB_FILES = ./src/functions.f90 ./src/linear_regression.f90 ./src/logistic_regression.f90 ./src/base_layer.f90 ./src/linear_layer.f90

test_regressions:
	gfortran ${COMPILER_TAGS} ${LIB_FILES} ./tests/test_regressions.f90 -o ./test_regressions
	./test_regressions


test_functions:
	gfortran ${COMPILER_TAGS} ${LIB_FILES} ./tests/test_functions.f90 -o ./test_functions
	./test_functions


test_layers:
	gfortran ${COMPILER_TAGS} ${LIB_FILES} ./tests/test_layers.f90 -o ./test_layers
	./test_layers


test_clean:
	rm ./test_regressions
	rm ./test_functions
	rm ./test_layers

test: test_functions test_regressions test_layers test_clean

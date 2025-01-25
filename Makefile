COMPILER_TAGS = -g -fcheck=all -Wall
LIB_FILES = ./src/functions.f08 ./src/linear_regression.f08 ./src/logistic_regression.f08

test_regressions:
	gfortran ${COMPILER_TAGS} ${LIB_FILES} ./tests/test_regressions.f08 -o ./test_regressions
	./test_regressions
	rm ./test_regressions

test_functions:
	gfortran ${COMPILER_TAGS} ${LIB_FILES} ./tests/test_functions.f08 -o ./test_functions
	./test_functions
	rm ./test_functions


test: test_functions test_regressions


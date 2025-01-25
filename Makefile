test:
	gfortran ./src/functions.f08 ./src/linear_regression.f08 ./src/logistic_regression.f08 ./tests/test_regressions.f08 -g -fcheck=all -Wall -o ./test_reg && ./test_reg

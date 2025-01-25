test:
	gfortran ./src/linear_regression.f08 ./src/logistic_regression.f08 ./test.f08 -g -fcheck=all -Wall -o ./test_reg && ./test_reg

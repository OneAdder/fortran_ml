test:
	gfortran ./src/linear_regression.f08 ./test.f08 -g -fcheck=all -Wall -o ./lin_reg && ./lin_reg

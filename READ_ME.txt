## Read me file


# How to compile program with gsl:
USER_NAME: sovereign
$ gcc -Wall -I/home/USER_NAME/gsl/include -c gsl_example.cpp 
$ gcc -L/home/USER_NAME/gsl/lib gsl_example.o -lgsl -lgslcblas -lm -o example

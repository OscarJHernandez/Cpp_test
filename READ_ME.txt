## Read me file


# How to compile program with gsl:
USER_NAME: sovereign
$ gcc -Wall -I/home/USER_NAME/gsl/include -c gsl_example.cpp 
$ gcc -L/home/USER_NAME/gsl/lib gsl_example.o -lgsl -lgslcblas -lm -o example


# Note: In order to compile, the following was very useful to use: 'pkg-config'
pkg-config --cflags --libs gsl
* This will notify you of any special flags that you must use in order to link or use the library


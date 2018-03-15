# How to compile without gsl libraries (example):



# How to compile with gsl Library (example):

Note: for the ubuntu 16.04, the gsl headers were placed in src, therefore, that is where 
I have specified to the c++ program to look.

/ To create an object file
$ gcc -Wall -I/usr/src -c integration_with_gsl.cpp 

/ To create the executable
$  gcc -L/usr/src integration_with_gsl.o -o outputName out -lgsl -lgslcblas -lm


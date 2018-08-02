# Example 2 for using c++ and fortran
This program calls a function which is inside a fortran module object


### To Compile the mixed Program
$ gfortran -c harmonic_oscillator.f90
$ g++ -c main.cpp
$ g++ harmonic_oscillator.o main.o -lstdc++ -o main
$ g++ harmonic_oscillator.o main.o -lstdc++ -o main -lgfortran -lquadmath

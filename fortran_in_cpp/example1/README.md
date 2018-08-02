# Example 1 for using c++ and fortran
This program simply multiplies two matrices using fortran within the c++ code


### To Compile the mixed Program
$ gfortran -c fortran_matrix_multiply.f90
$ g++ -c main.cpp
$ gfortran fortran_matrix_multiply.o main.o -lstdc++ -o mix_example.out

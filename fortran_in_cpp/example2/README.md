# Example 2 for using c++ and fortran
This program calls a function which is inside a fortran module object


### To Compile the mixed Program
$ gfortran -c fortran_matrix_multiply.f90
$ g++ -c main.cpp
$ gfortran fortran_matrix_multiply.o main.o -lstdc++ -o mix_example.out

#### Alternatively you can compile with:
$ g++ fortran_matrix_multiply.o main.o -lstdc++ -o mix_example.out

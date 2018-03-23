# How to compile without gsl libraries (example):



# How to compile with gsl Library (example):

Note: for the ubuntu 16.04, the gsl headers were placed in src, therefore, that is where 
I have specified to the c++ program to look.

* To create an object file  
$ g++ -Wall -I/usr/src -c integration_with_gsl.cpp 

* To create the executable  
$ g++ -L/usr/src integration_with_gsl.o -o outputName -lgsl -lgslcblas -lm


# Note: Use g++ over gcc
$ g++ name_of_program.cc -o executable_name



## How to use .h files for functions:

* Example:   

function1.c
```cpp
//=================================================================================
#include "function1.h"

// Simply adds the two inputs
int function1(int a, int b){
	return a+b;	
	}
//=================================================================================
```   

function1.h 
```cpp
//=================================================================================
#ifndef _function1
#define _function1

int function1(int,int);

#endif
//=================================================================================
```    

main.cpp
```cpp
//=================================================================================
using namespace std;
#include <iostream>
#include "function1.h"

int main(){
	cout << function1(1,2) << endl;
	
	return 0;
	}
//=================================================================================
```   

$ g++ -c function1.c
$ g++ -c main.cpp
$ g++ function.o main.o -o main


## How to use .h files for classes:










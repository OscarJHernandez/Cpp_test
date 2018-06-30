#include <pybind11/pybind11.h>
#include <pybind11/numpy.h>
#include <pybind11/eigen.h>
#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_math.h>
#include "funcs.cpp"
#include <Eigen/LU>

// Compile manually with:
// c++ -O3 -Wall -shared -I/home/sovereign/anaconda/include/python3.5m/ -I/home/sovereign/Documents/eigen/ -std=c++11 -fPIC example.cpp -o example`python3-config --extension-suffix` -lgsl -lgslcblas -lm
// c++ -O3 -Wall -shared -I/home/sovereign/anaconda/include/python3.5m/ -I/home/sovereign/Documents/eigen/ -std=c++11 -fPIC example.cpp  -o example.so -lgsl -lgslcblas -lm

namespace py = pybind11;

Eigen::MatrixXd inv(Eigen::MatrixXd xs) {
    return xs.inverse();
}

double det(Eigen::MatrixXd xs) {
    return xs.determinant();
}

double bessel_J0(double x)
{
 double y = gsl_sf_bessel_J0 (x);
 printf ("J0(%g) = %.18e\n", x, y);
 return y;
}

int add(int i, int j) {
    return i + j;
}

PYBIND11_MODULE(example, m) {
    m.doc() = "pybind11 example plugin"; // optional module docstring

    m.def("add", &add, "A function which adds two numbers");
    
    m.def("bessel_J0",&bessel_J0, "the simple bessel function");

    m.def("square",&square, "the simple x**2 function");
    
    m.def("inv", &inv);
    m.def("det", &det);
}

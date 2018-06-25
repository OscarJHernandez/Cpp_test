#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

namespace py = pybind11;
using namespace pybind11::literals;

int add(int i, int j) {
    return i + j;
};


// This example was used to test vectorizing a function
double square(double x){
	return x*x;
	}

// The buffer_info struct has the following shape:

//struct buffer_info {
    //void *ptr;
    //size_t itemsize;
    //std::string format;
    //int ndim;
    //std::vector<size_t> shape;
    //std::vector<size_t> strides;
//};	
	
// Passing in an array of doubles, change the values of the array
void twice(py::array_t<double> xs) {
    py::buffer_info info = xs.request();
    auto ptr = static_cast<double *>(info.ptr);

    int n = 1;
    for (auto r: info.shape) {
      n *= r;
    }

    for (int i = 0; i <n; i++) {
        *ptr++ *= 2;
    }
}


// Passing in a generic array
double sum(py::array xs) {
    py::buffer_info info = xs.request();
    auto ptr = static_cast<double *>(info.ptr);

    int n = 1;
    for (auto r: info.shape) {
      n *= r;
    }

    double s = 0.0;
    for (int i = 0; i <n; i++) {
        s += *ptr++;
    }

    return s;
}


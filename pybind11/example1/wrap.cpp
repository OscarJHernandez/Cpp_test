#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>
#include "funcs.hpp"

namespace py = pybind11;
using namespace pybind11::literals;




PYBIND11_PLUGIN(wrap) {
    py::module m("wrap", "pybind11 example plugin");
    
    // a simple function in c++
    m.def("add", &add, "A function which adds two numbers",
          "i"_a=1, "j"_a=2);
          
    // a simple function vecotorized in c++
    m.def("square", py::vectorize(square));
    
    // A function that takes a double numpy array as input
    m.def("sum", &sum);
    
    // A function that takes generic numpy array as input
    m.def("twice", &twice);
    
    
    return m.ptr();
}

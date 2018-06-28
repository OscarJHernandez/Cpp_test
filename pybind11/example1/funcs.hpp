#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

namespace py = pybind11;
using namespace pybind11::literals;


int add(int i, int j);
double square(double x);
void twice(py::array_t<double> xs);
double sum(py::array xs);
double sf_bessel(double x);

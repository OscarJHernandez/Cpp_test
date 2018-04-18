char const* greet()
{
   return "hello, world";
}

int add(int a, int b){
    return a+b;
    }
    
double product(double a, double b){
    return a*b;
    }
    
double divide(double a, double b){
    return a/b;
    }    
    
    

#include <boost/python.hpp>

BOOST_PYTHON_MODULE(hello_ext)
{
    using namespace boost::python;
    def("greet", greet);
    def("add", add);
    def("product", product);
    def("divide", divide);
}

#include <iostream>

using namespace std;

extern "C" {
	// To call a function inside a module, use the following naming convention:
	// __name_MOD_subname()
double __mod_MOD_func(double * x);
}


int main(){
	double x = 3.0;
	
	std::cout << "x" << std::endl;
	std::cout << __mod_MOD_func(&x) << std::endl;
	
	return 0;
	}

#include <gsl/gsl_integration.h>
#include <math.h> 
#include <iostream>

// Compile example using: g++ -o main main.cpp  -lgsl -lgslcblas -lm



struct my_params{
	double a;
	double b;
	double c;
	};

double f(double x, void * args){
	struct my_params * params = (struct my_params *)args;
	double a = (params -> a);
	double b = (params -> b);
	double c = (params -> c);
	
	return a*pow(x,b)+c;
	}


int main(){
	
	gsl_integration_workspace * w = gsl_integration_workspace_alloc (1000);
	double result, error;
	double alpha = 1.0;
	double a,b;
	struct my_params params = { 1.0, 3.0, 1.0 };
	
	a = 0.0;
	b = 1.0;
	gsl_function F;
	
	F.function = &f;
	F.params = &params;

	gsl_integration_qag (&F, a, b,
					 0.0, 1e-7, 1000,
					 GSL_INTEG_GAUSS15,
					 w,
					 &result, &error);

	std::cout << "Integration: " << result << std::endl;
	std::cout << "Integration Error: " << error << std::endl;
		
	return 0;
	}

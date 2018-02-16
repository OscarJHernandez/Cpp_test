#include <iostream>

using namespace std;

//
// The class containing the integral object
//
class Integral{
	
	// The class has access to these variables
	private: 
	int steps;
	double tolerance;
	double a;
	double b;
	
	public:
	
	// The Parametrized constructor for the class
	Integral(double a1, double b1, int steps1, double tol)
	{
		a = a1;
		b = b1;
		steps = steps1;
		tolerance = tol;
		}
	
	//-----------------------------------------
	// Define a simple Euler integration routine
	//-----------------------------------------
	double euler_integration(double (*f)(double)){
		double dh;
		double s;
		double xi;
		
		dh = (b-a)/(double)steps;
		s = 0.0;
		
		for(int i=0; i<= steps; i=i+1){
			xi = a+((double)i)*dh;
			s = s + dh*f(xi);
			}
		
		return s;
		}
	
	
	};
	
// The Function that we wish to integrate
double func(double x)
{
return x;
}
	
int main(){
	double a;
	double b;
	int steps;
	double tol;
	
	a = 0.0;
	b = 1.0;
	tol = 0.001;
	steps = 10000.0;
    
    // Instantiate the object	
	Integral Int1(a,b,steps,tol);

	cout << "Euler Integration of Function: " << Int1.euler_integration(func) << endl;
	return 0;
	}



#include <iostream>
#include "linear_regression.h"
using namespace std;

int main()
{
    int Ndata;
    Ndata = 500;
    linear_regression line1(Ndata);
    cout << "Hello world" << endl ;
    
    cout << "Ndata: " << line1.GetNdata() << endl;
    
    return 0;
    }

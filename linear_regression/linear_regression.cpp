#include "linear_regression.h"
#include <iostream>
using namespace std;

// The constructor for the Linear Regression object
linear_regression::linear_regression(int N)
{
    // Set the number of data points
    SetData(N);
    }

// The helper function for the class
void linear_regression::SetData(int N)
{
    Ndata = N;
    cout << "Function initialized" << endl;
    cout << "The number of data points is: " << Ndata << endl;
    }

int linear_regression::GetNdata()
{
    return Ndata;
    }

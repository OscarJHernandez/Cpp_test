#include <iostream>
using namespace std;


// This prints out the arrays
void count_elements(double array[], int len){
    
    for(int n=0; n < len; n++){
        cout << array[n] << endl;
        }
    
    }


int main()
{
    int Ndata;
    double A[] = {1.0,2.0,3.0,.0};
    char name[] = "test.dat";
    
    count_elements(A,4);
    
    cout << A << endl;
    
    return 0;
    }

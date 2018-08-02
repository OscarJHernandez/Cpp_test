#include <iostream>

using namespace std;

// Call the fortran module
extern"C" {
void fortfunc_(int *ii, float *ff);
void commons_to_angmom(void );
}

main()
{

   int ii=5;
   float ff=5.5;

   commons_to_angmom();

   return 0;
}

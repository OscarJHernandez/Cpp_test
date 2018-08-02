#include <iostream>

using namespace std;

// Call the fortran module
extern"C" {
void __ang_mom_functions_MOD_commons_to_angmom();
double __ang_mom_functions_MOD_gmosh(int* n,int* l,int* nc,int* lc,int* n1,int* l1,int* n2,int* l2,int* lr,double* d);
}

main()
{

   int ii=5;
   float ff=5.5;
   int n =0;
   int l = 0;
   int nc = 0;
   int lc = 0;
   int n1 = 0;
   int n2 = 0;
   int l1 = 0;
   int l2 = 0;
   int lr= 0;
   double d=1.0;
   
   __ang_mom_functions_MOD_commons_to_angmom();
   
   cout<<  __ang_mom_functions_MOD_gmosh(&n,&l,&nc,&lc,&n1,&l1,&n2,&l2,&lr,&d) << endl;

   return 0;
}

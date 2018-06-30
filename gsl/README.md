The following guide is taken from:
https://astrointro.wordpress.com/2017/05/17/installing-gnu-scientific-library-gsl-in-ubuntu-16-04-and-compiling-codes/

GNU Scientific Library is a nice package for astronomers and science community is general. It can perform a wide variety of numerical tasks like integration, special function computation etc. This library uses double precision by default and therefore should be useful for precise numerical computations. This is a step by step guide to installing GSL and compiling your first C code with GSL. The official page of this amazing project is https://www.gnu.org/software/gsl/

So let us get started !

## Step 1

Fetch GSL tarball from GSL website or the FTP given by

ftp://ftp.gnu.org/gnu/gsl/

I use the gsl-2.3, given at the end of the list, which works in Ubuntu 16.04 very well.

## Step 2

Unzip the tarball with archive manager and go to the folder that is created. It will be something like gsl-2.3. Open a terminal and cd to this folder.

## Step 3

Run the following commands in the terminal.

./configure
make
sudo make install
The process can take a few minutes to complete. Especially the make process.

##Step 4

We are almost done ! However linking to the newly installed library can be a bit cumbersome while compiling. Therefore, add the following lines to your .bashrc file in home folder (this is hidden usually, so do Ctrl+H to see it)

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib/

##Step 5

Now let us create a proper test C file and test our GSL ! Open a my_test.c file in a folder of you choice and your favorite editor (gedit, vim etc) and copy paste the following code. This has been taken from the official GSL page.

```
#include <stdio.h>
#include <gsl/gsl_sf_bessel.h>
#include <gsl/gsl_math.h>

int main (void)
{
 double x = 5.0;
 double y = gsl_sf_bessel_J0 (x);
 printf ("J0(%g) = %.18e\n", x, y);
 return 0;
}
```
Save and exit.

##Step 6

Open a terminal and cd to the folder where you saved the my_test.c code. Run the following command to compile the code.

gcc -o my_test.out my_test.c -lgsl -lgslcblas -lm
If everything goes well, you will see a my_test.out created. Run it with the following command.

./my_test.out
The result should be.

J0(5) = -1.775967713143382642e-01
Bravo ! You now have a working GSL installation. A guide to using some of the functions in this library is planned for upcoming posts.

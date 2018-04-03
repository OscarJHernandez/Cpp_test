#include <stdio.h>
#include <gsl/gsl_rng.h> // Import the GSL random number generator


#define NP 100


int main(){
    
    gsl_rng *r; // The global generator
    
    double p[NP];
    int i;
    //const gsl_rng_type * T;
    unsigned long seed = 10UL;
    
    gsl_rng_env_setup();
    
    // T = gsl_rng_default;
    r = gsl_rng_alloc(gsl_rng_taus2); // gsl_rng_alloc(T);
    
    gsl_rng_set(r, seed);
    
    for(i=0; i < NP; i++){
        p[i] = gsl_rng_uniform(r);
        printf("p[%i]: %.3g\n",i,p[i]);
        }
    
    printf("1UL: %lu",seed);
    //printf ("generator type: %s\n", gsl_rng_name (r));
    //printf ("seed = %lu\n", gsl_rng_default_seed);
    //printf ("first value = %lu\n", gsl_rng_get (r));

    gsl_rng_free (r);
    return 0;
    }

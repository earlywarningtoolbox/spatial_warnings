#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_sf_expint.h>

/* Calculate the exponential integral function by invoking the GNU scientific
   library */
/* Only intended to be used with R; not the most elegant integration but it
   may do for now */

/* One argument, the value (double) at which to evaluate Ei(x) */

double x, ei; /* argument to exponential integral, value */
char *program_name; /* name program is invoked under, for errors */

main(int argc, char* argv[]) {
  void usage(void);	/* Warn users about proper usage */

  program_name = argv[0];
  if (argc != 2) {
    usage();
  }
  x = atof(&argv[1][0]);
  ei = gsl_sf_expint_E1(x);
  printf("%.18e\n",ei);
  return(0);
}

void usage(void) {
  (void) fprintf(stderr, "Usage is %s [floating-point argument]\n", program_name);
  exit(8);
}

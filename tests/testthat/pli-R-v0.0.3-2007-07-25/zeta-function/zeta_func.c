#include <stdio.h>
#include <stdlib.h>
#include <gsl/gsl_sf_zeta.h>

/* Calculate the Hurwitz zeta function by invoking the GNU scientific
   library */
/* Only intended to be used with R; not the most elegant integration but it
   may do for now */

/* Two arguments, the power used in the sum (s) and the baseline value (q) */

double s, q, zeta; /* arguments to zeta, value */
char *program_name; /* name program is invoked under, for errors */

main(int argc, char* argv[]) {
  void usage(void);	/* Warn users about proper usage */

  program_name = argv[0];
  if (argc != 3) {
    usage();
  }
  s = atof(&argv[1][0]);
  if (s <= 1.0) {
    usage();
  }
  q = atof(&argv[2][0]);
  if (q <= 0.0) {
    usage();
  }
  zeta = gsl_sf_hzeta(s,q);
  printf("%.18e\n",zeta);
  return(0);
}

void usage(void) {
  (void) fprintf(stderr, "Usage is %s [floating-point exponent > 1] [floating-point additive constant > 0]\n", program_name);
  exit(8);
}

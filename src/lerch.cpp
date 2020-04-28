// 
// This function will compute the lerch phi function. It is a simplified 
// version of what is done in the VGAM source code (© Thomas Yee), which was 
// originally written by 
// 
// Sergej V. Aksenov (http://www.geocities.com/saksenov) and 
// Ulrich D. Jentschura (jentschura@physik.tu-dresden.de), 2002.
// 
// 

#include <Rcpp.h>

using namespace Rcpp; 

const int MAXIT = 1000; 
const double TOL = 1e-10; 

//[[Rcpp::export]]
long int lerchphi(double z, double s, long int v) { 
  
  // If z is above 1.0, the sum diverges. Return Inf
  if ( z > 1.0 ) { 
    return(R_PosInf); 
  }
  
  // If z *is* 1.0, but s is below one, then the series diverges
  // CASE NOT HANDLED
  
  // If v is below zero, the function is undefined 
  if ( v <= 0.0 ) { 
    return(R_NaN); 
  }
  
  // In other cases, we proceed with computing the sum
  double total = 0; 
  for (int k=1; k<=MAXIT; k++) { 
    double sumterm = pow(z, k) / pow(k + v, s); 
    total+= sumterm; 
    if ( (sumterm/total) < TOL ) { 
      break; 
    }
  }
  
  return(total); 
}


// 
// Compute the skewness of a set of values
// 

#include <Rcpp.h>
using namespace Rcpp; 

#define NOVAR_SKEWNESS_VAL NA_REAL

//[[Rcpp::export]]
double cpp_skewness(Rcpp::NumericVector X) { 
  
  // Number of elements in X
  int N = X.size(); 
  
  // if not enough elements, return early 
  if ( N < 2 ) { 
    return( NOVAR_SKEWNESS_VAL ); 
  }
  
  // else, proceed with computing skewness
  double xmean = mean(X); 
  
  // Compute the upper term
  double cubesum = 0; 
  double sqsum = 0; 
  for (int i=0; i<N; i++) { 
    cubesum += (X(i) - xmean) * (X(i) - xmean) * (X(i) - xmean); 
    sqsum  += (X(i) - xmean) * (X(i) - xmean); 
  }
  cubesum = cubesum / N; 
  sqsum  = sqsum  / N; 
  
  // Compute skewness
  double skewness; 
  if ( sqsum == 0 ) { 
    return( NOVAR_SKEWNESS_VAL ); 
  } else { 
    skewness = cubesum / pow(sqsum, 1.5); 
  }
  return(skewness);
}

// 
// A small function that computes moran's I index as it was done in the 
//   equivalent function moran_computation() (./R/indicator_moran.R).
// 

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double moranCpp(NumericMatrix mat) { 
  
  double m = mean(mat);
  double v = var(mat);
  int h = mat.nrow() - 1;
  int w = mat.ncol() - 1;

  double moranI = 0;
  for (int i=1; i<h; i++) { 
    for (int j=1; j<w; j++) { 
      moranI += (mat(i,j) - m) * (mat(i,j-1) + mat(i,j+1) + mat(i-1,j) + 
                                  mat(i+1,j) - 4*m);
    }
  }
  
  moranI = moranI / (4 * v * (h-2) * (w-2));
  
  return(moranI);
}

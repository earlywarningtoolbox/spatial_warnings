// 
// A small function that computes moran's I index 
// 

#include <Rcpp.h>
using namespace Rcpp;

//' 
//' @title Compute the Moran's I at lag 1
//' 
//' @param mat A matrix
//' 
//' @description This function computes the Moran'I value at lag 1.
//'
//' @details See \link{indicator_moran} or \link{generic_spews} 
//'   for more information
//'
//' @return The Moran's I numeric value as a numeric number.
//' 
//' @seealso \link{indicator_moran}, \link{generic_spews} 
//' 
//' @export
//[[Rcpp::export]]
double raw_moran(NumericMatrix mat) { 
  
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

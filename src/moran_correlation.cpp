// 
// A small function that computes moran's I index 
// 

#include <RcppArmadillo.h>
using namespace arma;

//' 
//' @title Spatial correlation at lag 1
//'
//' @description This function computes the Moran's I index of spatial 
//'   correlation at lag 1.
//' 
//' @param mat A matrix
//' 
//' @return The Moran's I numeric value as a numeric number.
//' 
//' @details This function returns the spatial correlation as measured by 
//'   the Moran's I index. If the variance of the matrix is zero, then 
//'   \code{NaN} is returned. 
//' 
//' @seealso \code{\link{indicator_moran}}, \code{\link{generic_sews}} 
//' 
//' @examples
//' 
//' # Spatial correlation of white noise is close to zero
//' rmat <- matrix(runif(1000) > .5, ncol = 100)
//' raw_moran(rmat) 
//' 
//'@export
//[[Rcpp::export]]
double raw_moran(arma::mat& mat) { 
  
  double m = mean(vectorise(mat));
  double v = var(vectorise(mat)); // Treats mat as vector
  
  uword h = mat.n_rows - 1;
  uword w = mat.n_cols - 1;
  
  
  double moranI = 0;
  for (uword j=1; j<w; j++) { 
    for (uword i=1; i<h; i++) { 
      moranI += (mat(i,j) - m) * (mat(i,j-1) + mat(i,j+1) + mat(i-1,j) + 
                                  mat(i+1,j) - 4*m);
    }
  }
  
  moranI = moranI / (4 * v * (h-2) * (w-2));
  
  return(moranI);
}

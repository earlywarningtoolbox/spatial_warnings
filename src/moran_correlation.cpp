// 
// A small function that computes moran's I index 
// 

#include <Rcpp.h>
using namespace Rcpp;

//' 
//' @title Compute the Moran's I at lag 1
//'
//' @description This function computes the Moran'I value at lag 1.
//' 
//' @param mat A matrix
//'
//' @return The Moran's I numeric value as a numeric number.
//' 
//' @seealso \code{\link{indicator_moran}}, \code{\link{generic_spews}} 
//' 
//' @examples
//' 
//' rmat <- matrix(runif(1000) > .5, ncol = 100)
//' raw_moran(rmat) # close to zero
//' 
//'@export
//[[Rcpp::export]]
double raw_moran(NumericMatrix mat) { 
  
  double m = mean(mat);
  double v = var(mat); // Treats mat as vector
  
  int h = mat.nrow() - 1;
  int w = mat.ncol() - 1;
  
  
  double moranI = 0;
  for (int j=1; j<w; j++) { 
    for (int i=1; i<h; i++) { 
      moranI += (mat(i,j) - m) * (mat(i,j-1) + mat(i,j+1) + mat(i-1,j) + 
                                  mat(i+1,j) - 4*m);
    }
  }
  
  moranI = moranI / (4 * v * (h-2) * (w-2));
  
  return(moranI);
}

/*
// Spatial correlation as in Eby et al. (see ./draft/cg-indicators.cpp)
// 
// Useless but kept here for reference -> returns the same results as Moran's I
double corr_lag1(NumericMatrix mat) { 
  
  double sum=0;
  int i,j;
  int down, right;
  int size = mat.nrow(); // Assume square mat
  double matvar  = var(mat); // Treats mat as vector
  double matmean = mean(mat); 
  Rcout << matmean << "/" << matvar << "\n"; 
  
  for (i=0; i<size; i++) {
    for (j=0; j<size; j++) {
      
      down  = (i+1) % size; 
      right = (j+1) % size; 
      
      sum = sum + ( mat(i, j) * mat(down, j) ) 
                + ( mat(i, j) * mat(i, right));
      
    }
  }
  
  Rcout << sum << "\n"; 
  double corLag1;
  if (matvar == 0) { 
    corLag1 = 0;
  } else { 
    corLag1 = (sum / (2*size*size) - matmean * matmean ) / matvar;
  }
  
  return(corLag1); 
}

*/

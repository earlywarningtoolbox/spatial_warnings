// 
// 
// Function that takes care of coarse-graining
// 
// Function to reduce original matrix to submatrix by averaging
// This function is used bt the generic indicators variance, skewness and moran 
// correlation. It divides the square matrix into submatrices of mean cover.
// 
// Written by Vishwesha Guttal, 7th Nov 2013.
// Modified by Sabiha Majumder
// Converted to c++ by Alexandre Génin
//

#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
NumericMatrix coarse_grain_cpp(NumericMatrix mat, 
                           int subsize) {
  
  int N = mat.nrow();
  int n = floor(N / subsize); 
  
  NumericMatrix reduced_matrix = NumericMatrix(n, n);
  
  // Fill in values of the submatrix
  for (int j=0; j<n; j++) { 
    for (int i=0; i<n; i++) {
      
      // Compute mean of the corresponding cells in the original matrix
      double sum = 0;
      for ( int k=(i*subsize); k< (i+1)*subsize; k++ ) { 
        for ( int l=(j*subsize); l< (j+1)*subsize; l++ ) { 
          sum += mat(k,l);
          // Rcout << k << "-" << l << ":" << mat(k,l) << "->" << sum << std::endl;
        }
      }
      
      reduced_matrix(i,j) = sum / ( subsize * subsize ) ;
    }
  }
  
  return reduced_matrix;
}

// 
// 
// This file contains a function that will compute the r-spectrum
// 

#include <RcppArmadillo.h>

using namespace Rcpp; 
using namespace arma; 


// [[Rcpp::export]]
DataFrame rspectrum(NumericMatrix rmat) { 
  
  // Convert input matrix to mat
  mat amat(rmat.begin(), 
           rmat.nrow(), rmat.ncol(), 
           false); // reuses memory and avoids extra copy
  
  int nr = amat.n_rows; 
  int nc = amat.n_cols; 
  
  // Middle point of matrix
  int n0x = floor(nc/2); 
  int n0y = floor(nr/2); 
  
  // Minimum and maximum distances to consider
  int mi = 1;
  int ma = 1 + (n0x < n0y ? n0x : n0y);
  
  // Compute and shift DFT
  cx_mat mat_fft = fft2(amat); 
  mat_fft = shift(mat_fft, n0x, 0); 
  mat_fft = shift(mat_fft, n0y, 1); 
  
  // Compute aspectr2D and normalize it
  mat_fft(n0x, n0y) = 0; 
  mat aspectr2D = pow(abs(mat_fft), 2) / pow((n0x+1) * (n0y+1), 4);

  // Compute r-spectrum
  vec ray = linspace(mi, ma, ma-mi+1);
  vec rspectr = zeros(ma-mi+1); 
  double step = 1;
  double norm_factor = 0; 
  
  // For all lengths
  for (int l=0; l<ray.n_elem; l++) { 
    double r = ray(l);
    
    // Go through the distances matrix and make a sum of the relevant ones
    double total_inmask = 0;
    for (int i=0; i<nr; i++) { 
      for (int j=0; j<nc; j++) { 
        
        double dist = sqrt( pow(i - n0x, 2) + pow(j - n0y, 2) );
        
        if ( dist >= r - step/2 && dist < r + step/2 ) { 
          rspectr(l) += aspectr2D(i, j);
          norm_factor += aspectr2D(i, j);
          total_inmask++;
        }
        
      }
    }
    rspectr(l) = rspectr(l) / total_inmask;
  }
  rspectr = rspectr / norm_factor;
  
  return DataFrame::create(_["dist"]  = ray, 
                           _["rspec"] = rspectr);
}


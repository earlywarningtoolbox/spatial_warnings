// 
// 
// This file contains a function that will compute the r-spectrum
// 

#include <RcppArmadillo.h>

using namespace Rcpp; 
using namespace arma; 


//
//' @export
// [[Rcpp::export]]
DataFrame rspectrum(NumericMatrix rmat) { 
  
  // Convert input matrix to arma-understandable stuff
  mat amat(rmat.begin(), 
           rmat.nrow(), rmat.ncol(), 
           false); // reuses memory and avoids extra copy
  
  
  // Get number of rows and number of cols
  int nr = amat.n_rows; 
  int nc = amat.n_cols; 
  
  // Middle point of matrix
  int n0x = floor(nc/2); 
  int n0y = floor(nr/2); 
  
  // Minimum and maximum distances to consider
  int mi = 1;
  int ma = 1 + (n0x < n0y ? n0x : n0y);

  // Initialize the variables that will hold the spectrum values
  vec ray = linspace(mi, ma, ma-mi+1);
  vec rspectr = zeros(ma-mi+1); 
  
  // We check whether there is more than one value in the supplied matrix
  bool more_than_two_values = false;
  int i=1;
  while ( !more_than_two_values && i<amat.n_elem ) {
    if ( amat(i-1) != amat(i) ) { 
      more_than_two_values = true;
    }
    i++;
  } 
  
  // We have not found more than two values -> return early
  if ( !more_than_two_values ) { 
    return DataFrame::create(_["dist"]  = ray, 
                             _["rspec"] = NumericVector::create(NumericVector::get_na()));
  }
  
  // Compute and shift DFT
  cx_mat mat_fft = fft2(amat); 
  
  // Compute aspectr2D and normalize it
  mat_fft(n0x, n0y) = 0; 
  
  // Compute r-spectrum
  double step = 1;
  double norm_factor = 0; 
  double aspectr2D_ij, dist;
  int total_inmask, shift_i, shift_j;
  
  // For all lengths
  for (int l=0; l<ray.n_elem; l++) { 
    double r = ray(l);
    
    // Go through the distances matrix and make a sum of the relevant ones
    total_inmask = 0;
    for (int i=0; i<nr; i++) { 
      for (int j=0; j<nc; j++) { 
        
        dist = sqrt( pow(i - n0x, 2) + pow(j - n0y, 2) );
        
        if ( dist >= r - step/2 && dist < r + step/2 ) { 
          
          // We use shifted coordinates to pick the value in mat_fft 
          // instead of making a call to arma::shift() that does a copy of 
          // the matrix
          shift_i = (i + n0x) % nr;
          shift_j = (j + n0y) % nc;
          aspectr2D_ij = pow(abs(mat_fft.at(shift_i, shift_j)), 2) / pow((n0x+1) * (n0y+1), 4);
          
          rspectr(l) += aspectr2D_ij;
          norm_factor += aspectr2D_ij;
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


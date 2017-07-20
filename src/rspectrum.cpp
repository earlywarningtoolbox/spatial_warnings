// 
// 
// This file contains a function that will compute the r-spectrum
// 

#include <RcppArmadillo.h>

using namespace Rcpp; 
using namespace arma; 

#define SQ(a) ( (a) * (a) )

// Distance step of the r-spectrum
#define step 1.0

//
// [[Rcpp::export]]
DataFrame rspectrum(arma::mat amat) { 
  
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
  vec ray = linspace(mi, ma, ma-mi+1); // +1 ?
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
  double norm_factor = 0; 
  
  // For all lengths
  for (int l=0; l<ray.n_elem; l++) { 
    double r = ray(l);
    
    // Go through the matrix and make a sum of relevant fft coefficients
    int total_inmask = 0;
    
    // Note that we do not loop over cells that will not be in the correct 
    // distance range. 
    for (int j=(n0y - r - 1); j<(n0y + r + 1); j++) { 
      for (int i=(n0x - r - 1); i<(n0x + r + 1); i++) { 
        
        // Squared distance to center
        double dist = SQ(i-n0x) + SQ(j-n0y);
        
        if ( dist >= SQ(r - step/2) && dist < SQ(r + step/2) ) { 
          
          // We use shifted coordinates to pick the value in mat_fft 
          // instead of making a call to arma::shift() that does a copy of 
          // the matrix
          int shift_i = (i + n0x) % nr;
          int shift_j = (j + n0y) % nc;
          double aspectr2D_ij = SQ( abs(mat_fft(shift_i, shift_j)) ) / 
                                  SQ(SQ( (n0x+1) * (n0y+1) ));
          
          rspectr(l) += aspectr2D_ij;
          norm_factor += aspectr2D_ij;
          total_inmask++;
        }
        
      }
    }
    
//     Rcout << "dist: " << r << " -> total_inmask: " << total_inmask << "\n"; 
    
    rspectr(l) = rspectr(l) / total_inmask;
  }
  
  rspectr = rspectr / norm_factor;
  
  return DataFrame::create(_["dist"]  = ray, 
                           _["rspec"] = rspectr);
}


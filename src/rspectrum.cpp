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
DataFrame rspectrum_cpp(arma::mat amat, 
                        int nthreads) { 
  
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
  double norm_factor = 0; 
  
  // For all lengths
  for (int l=0; l<ray.n_elem; l++) { 
    double r = ray(l);
    
    // Go through the distances matrix and make a sum of the relevant ones
    int total_inmask = 0;
    double rspectr_current = 0; 
    double normfactor_current = 0; 
#pragma omp parallel for collapse(2) num_threads(nthreads) \
  reduction(+:rspectr_current) \
  reduction(+:normfactor_current) \
  reduction(+:total_inmask)
    for (int j=0; j<nc; j++) { 
      for (int i=0; i<nr; i++) { 
        
        double dist = SQ(i-n0x) + SQ(j-n0y);
        
        if ( dist >= SQ(r - step/2) && dist < SQ(r + step/2) ) { 
          
          // We use shifted coordinates to pick the value in mat_fft 
          // instead of making a call to arma::shift() that does a copy of 
          // the matrix
          int shift_i = (i + n0x) % nr;
          int shift_j = (j + n0y) % nc;
          double aspectr2D_ij = SQ(abs(mat_fft(shift_i, shift_j))) / 
                                  SQ( SQ((n0x+1) * (n0y+1)) );
          
          rspectr_current += aspectr2D_ij;
          normfactor_current += aspectr2D_ij;
          total_inmask++;
        }
        
      }
    }
    
    rspectr(l) = rspectr_current / total_inmask;
    norm_factor += normfactor_current;
  }
  rspectr = rspectr / norm_factor;
  
  return DataFrame::create(_["dist"]  = ray, 
                           _["rspec"] = rspectr);
}


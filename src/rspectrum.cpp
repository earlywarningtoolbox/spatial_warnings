// 
// 
// This file contains a function that will compute the r-spectrum
// 

#include <RcppArmadillo.h>

using namespace Rcpp; 

#define SQ(a) ( (double)(a) * (double)(a) )

// Distance step of the r-spectrum
#define step 1.0

// 
//' @title r-spectrum 
//' 
//' @description Compute the r-spectrum of a matrix 
//' 
//' @param mat A matrix of logical or numeric values 
//' 
//' @return A data.frame with two columns: \code{dist}, the wave number and 
//'   \code{rspec}, the normalized value of the r-spectrum
//' 
//' @seealso \code{\link{spectral_spews}}, \code{\link{indicator_sdr}}
//' 
//' @export
// [[Rcpp::export]]
DataFrame rspectrum(arma::mat mat) { 
  
  // Get number of rows and number of cols
  int nr = mat.n_rows; 
  int nc = mat.n_cols; 
  
  // Middle point of matrix
  int n0x = floor(nc/2); 
  int n0y = floor(nr/2); 
  
  // Minimum and maximum distances to consider
  int mi = 1;
  int ma = 1 + (n0x < n0y ? n0x : n0y);

  // Initialize the variables that will hold the spectrum values
  arma::vec ray = arma::linspace(mi, ma, ma-mi+1); // +1 ?
  arma::vec rspectr = arma::zeros(ma-mi+1); 
  
  // We check whether there is more than one value in the supplied matrix
  bool more_than_two_values = false;
  int i=1;
  while ( !more_than_two_values && i<mat.n_elem ) {
    if ( mat(i-1) != mat(i) ) { 
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
  arma::cx_mat mat_fft = arma::fft2(mat); 
  
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



#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// This piece of code computes the flowlength before taking into account 
// the dimensions of pixels/slope. 
//[[Rcpp::export]]
double fl_internal(arma::mat m) { 
  
  uword ny = m.n_cols; 
  uword nx = m.n_rows; 
  
  rowvec flcol = zeros(1, ny); 
  for ( uword r=0; r<nx; r++ ) { 
    rowvec a = sum(cumprod(m.rows(r, nx - 1), 0), 0); 
    flcol += a; 
  } 
  
  return( accu(flcol)/((double)nx * (double)ny) ); 
}

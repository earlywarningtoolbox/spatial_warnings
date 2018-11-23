
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// This piece of code computes the cumulative product of a matrix by column, 
// then sums it by columns (similar to what sum(cumprod(mat)) does in matlab).
//[[Rcpp::export]]
arma::rowvec col_sumcumprod(arma::mat m) { 
  return( sum(cumprod(m,0), 0) ); 
}

//[[Rcpp::export]]
double fl_internal(arma::mat m) { 
  
  uword ny = m.n_cols; 
  uword nx = m.n_rows; 
  
  rowvec flcol = zeros(1, ny); 
  for ( uword r=0; r<nx; r++ ) { 
    rowvec a = sum(cumprod(m.rows(r, nx - 1), 0), 0); 
    flcol += a; 
  } 
  
  return( accu(flcol)/(nx*ny) ); 
}

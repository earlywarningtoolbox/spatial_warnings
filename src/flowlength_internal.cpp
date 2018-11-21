
#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// This piece of code computes the cumulative product of a matrix by column, 
// then sums it by columns (similar to what sum(cumprod(mat)) does in matlab).
//[[Rcpp::export]]
arma::rowvec col_sumcumprod(arma::mat m) { 
  return( sum(cumprod(m,0), 0) ); 
}

// 
// 
// This file provides functions to compute a null distribution
//   of values for an indicator.
// 

#include <RcppArmadillo.h>

using namespace arma;
using namespace Rcpp;

// Generate random integer between min and max 
#define RANDN(min, max) \
  (int)(unif_rand() * (max - min)) + min; 

// Do a Fisher-Yates shuffling in place 
// Algorithm from wikipedia: (Fisher-Yates shuffling)
// To shuffle an array a of n elements (indices 0..n-1):
//   for i from 0 to n − 2 (included) do
//        j ← random integer such that i ≤ j < n
//        exchange a[j] and a[i]
// 
void shuffle_matrix_internal(arma::mat& mat, 
                             arma::uword nr, 
                             arma::uword nc) {
  
  int kmax=(nc*nr)-2;

  for (int k = 0; k<=kmax; k++) { 
    // j between k and nc*nr 
    uword j = RANDN(k, nc*nr); 
    
    // As we are in a matrix we need to compute the 2D coordinates of the 
    //   cells to swap.
    uword xold = k/nc;
    uword xnew = j/nc;
    uword yold = k % nc;
    uword ynew = j % nc;
    
    // Swap the two cells
    double tmp;
    tmp = mat(xold, yold);
    mat(xold, yold) = mat(xnew, ynew);
    mat(xnew, ynew) = tmp;
  }
  
}

// Do a Fisher-Yates shuffling of a matrix and return the shuffled copy. 
// This function is not used normally but may be useful for testing. 
// 
//[[Rcpp::export]] 
arma::mat shuffle_matrix(arma::mat& mat) {
  arma::mat tmpmat = mat; 
  shuffle_matrix_internal(tmpmat, tmpmat.n_rows, tmpmat.n_cols); 
  return(tmpmat); 
}

//[[Rcpp::export]]
List shuffle_and_compute(arma::mat& mat, 
                         Function indic, 
                         int nrep) { 
  
  arma::mat shuffmat = mat; 
    
  // Allocate indicator results
  List nulldistr = List(nrep);
  
  // Get matrix size
  uword nr = mat.n_rows;
  uword nc = mat.n_cols;
  
  for (int i=0; i < nrep; i++) { 
    // Shuffle matrix and compute indicator value. Note that indicf is an R 
    // function and as a result is not thread safe. 
    shuffle_matrix_internal(shuffmat, nr, nc); 
    nulldistr(i) = indic( shuffmat );
  }
  
  return nulldistr;
}

/* 
 * 
 * # Check shuffle_matrix by using this R code : 
 *
 * library(plyr)
 * SIZE <- 6
 * res <- adply(seq.int(500), 1, function(elem) { 
 *     tmp <- matrix(seq.int(SIZE*SIZE), nrow = SIZE, ncol = SIZE)
 *     a <- matrix(seq.int(SIZE*SIZE), nrow = SIZE, ncol = SIZE)
 *     b <- shuffle_matrix(tmp) # this breaks R normal behavior !
 *     adply(seq.int(SIZE*SIZE), 1, function(i) { 
 *     data.frame(posin  = which(a == i), 
 *                 posout = which(b == i))
 *     }, .id = NULL)
 * }, .id = NULL, .progress = 'time', .parallel = FALSE)
 *
 * # Transform into frequencies 
 * res2 <- ddply(res, ~ posin, with, 
 *                   data.frame(posout = unique(res[ ,"posin"]), 
 *                             posout.freq = sapply(unique(res[ ,"posin"]), 
 *                                                   function(x) mean(posout == x))))
 * 
 * library(ggplot2)
 * ggplot(res2) + 
 *   geom_tile(aes(posin, posout, fill = posout.freq))
 * 
 * # All these distributions should be uniform (overlaid straight lines 
 * # in a qqplot)
 * ggplot(res2) + 
 *   geom_qq(aes(sample = posout, group = posin), 
 *           distribution = stats::qunif, geom = "line") 
 */

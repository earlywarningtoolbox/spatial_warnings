// // 
// // 
// // This file provides functions to compute a null distribution
// //   of values for an indicator.
// // 
// 


#include <Rcpp.h>
#include "headers.h"  // Needs to be included afeter Rcpp.h

using namespace Rcpp;


// Generate random integer between min and max (no bias)
//[[Rcpp::export]]
int randn(double min, double max) { 
  NumericVector result = runif(1, 0, 1);
  return floor( min + result[0] * ((max+1) - min) );
}

//[[Rcpp::export]]
List shuffle_and_compute(NumericMatrix mat, 
                         Function indic, 
                         bool do_coarse_grain,
                         int subsize,
                         int nrep) { 
  
  NumericMatrix shuffmat = mat; 
  NumericMatrix coarsemat = coarse_grain(mat, subsize);
  
  // Allocate indicator results
  List nulldistr = List(nrep);
  
  // Get matrix sizes
  int nr = mat.nrow();
  int nc = mat.ncol();
  
  for (int i=0; i < nrep; i++) { 
    
      
    for (int k = 0; k<=(nc*nr)-2; k++) { 
      int j = randn(i, (nc*nr)-1); // j between i and nc*nr - 1
      
      // As we are in a matrix we need to compute the 2D coordinates of the 
      //   cells to swap.
      int xold = floor(k/nc);
      int xnew = floor(j/nc);
      int yold = k % nc;
      int ynew = j % nc;
      
      // Swap the two cells
      double tmp;
      tmp = mat(xold, yold);
      mat(xold, yold) = mat(xnew, ynew);
      mat(xnew, ynew) = tmp;
    }
    
    // Coarse grain ?
    if ( do_coarse_grain ) { 
      coarsemat = coarse_grain(shuffmat, subsize); 
      nulldistr(i) = indic(coarsemat);
    // Do not
    } else { 
      nulldistr(i) = indic(shuffmat);
    }
    
  }
  
  return nulldistr;
}

// Wikipedia algorithm: (Fisher-Yates shuffling)
// To shuffle an array a of n elements (indices 0..n-1):
//   for i from 0 to n − 2 (included) do
//        j ← random integer such that i ≤ j < n
//        exchange a[j] an  d a[i]
// 
// Check shuffle_matrix using this R code : 
// 
// library(doParallel)
// library(plyr)
// registerDoParallel(cores = 2)
// res <- alply(seq.int(10*10)-1, 1, function(elem) { 
//   replicate(100000, { 
//     a <- matrix(seq.int(100)-1, nrow = 10, ncol = 10)
//     b <- shuffle_matrix(a)
//     which(b == elem)
//   })
// }, .progress = 'time', .parallel = TRUE)
// 
// res2 <- as.data.frame(res)
// library(tidyr)
// res3 <- gather(res2, input, output, X1:X100)
// # res3$input <- as.integer(res3$input)
// library(ggplot2)
// ggplot(res3) + 
//   geom_density(aes(output, color = input))
// 
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

// Shuffle a matrix in place (checked for absence of bias)
NumericMatrix shuffle_matrix(NumericMatrix &mat) { 
  
  int nr = mat.nrow();
  int nc = mat.ncol();
  double tmp;
  
  for (int i = 0; i<=(nc*nr)-2; i++) { 
    
    int j = randn(i, (nc*nr)-1); // j between i and nc*nr - 1
    
    // As we are in a matrix we need to compute the 2D coordinates of the 
    //   cells to swap.
    int xold = floor(i/nc);
    int xnew = floor(j/nc);
    int yold = i % nc;
    int ynew = j % nc;
    
    // Swap the two cells
    tmp = mat(xold, yold);
    mat(xold, yold) = mat(xnew, ynew);
    mat(xnew, ynew) = tmp;
  }
  
  return mat;
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
  
  for (int i=0; i < nrep; i++) { 
    // Shuffle matrix in place
    shuffmat = shuffle_matrix(shuffmat);
    
    // Coarse grain
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
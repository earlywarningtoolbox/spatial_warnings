// 
// 
// This file provides functions to compute a null distribution
//   of values for an indicator.
// 


#include <omp.h> // openMP
#include <Rcpp.h>
#include "headers.h"  // Needs to be included afeter Rcpp.h

using namespace Rcpp;

// Generate random integer between min and max (no bias)
#define RANDN(min, max, seed) \
  floor( min + ((double)(rand_r(&seed)) / RAND_MAX) * (max - min) )

//[[Rcpp::export]]
List shuffle_and_compute(NumericMatrix& mat, 
                         Function indic, 
                         int nrep, 
                         int nthreads) { 
  
  NumericMatrix shuffmat = mat; 
  
  // Allocate indicator results
  List nulldistr = List(nrep);
  
  // Get matrix size
  int nr = mat.nrow();
  int nc = mat.ncol();
  
  int kmax=(nc*nr)-2;
  
  for (int i=0; i < nrep; i++) { 
    
    // Shuffle the matrix. Note that we can only parallelize the shuffling as 
    //   the indicf function is not safe to be called from the threads. 
#pragma omp parallel for num_threads(nthreads)
    for (int k = 0; k<=kmax; k++) { 
      unsigned seed = 6653 + omp_get_thread_num()*2; 
      int j = RANDN(k, kmax-1, seed); 
      // j between k and nc*nr - 1
      
      // As we are in a matrix we need to compute the 2D coordinates of the 
      //   cells to swap.
      int xold = floor(k/nc);
      int xnew = floor(j/nc);
      int yold = k % nc;
      int ynew = j % nc;
      
      // Swap the two cells
      double tmp;
      tmp = shuffmat(xold, yold);
      shuffmat(xold, yold) = shuffmat(xnew, ynew);
      shuffmat(xnew, ynew) = tmp;
    }
    
    nulldistr(i) = indic(shuffmat);
    
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

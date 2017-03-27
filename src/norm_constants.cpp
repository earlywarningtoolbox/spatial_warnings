// 
// 
// This file contains a function that computes the normalizing constant of 
//   a TPL. The function is vectorized. 
// 

#include <Rcpp.h>
#define TOLERANCE 1e-64

using namespace Rcpp; 

//[[Rcpp::export]]
NumericVector tplsum(double expo, double rate, IntegerVector xs, int xmin) { 
  
  NumericVector output(xs.length());
  
  for (int i=0; i<xs.length(); i++) { 
    int x = xs(i);
    double total = 0;
    for (int k=xmin; k<x; k++) { 
      total += pow(k, -expo) * exp(-k*rate);
    }
    output(i) = total;
  }
  
  return(output);
}

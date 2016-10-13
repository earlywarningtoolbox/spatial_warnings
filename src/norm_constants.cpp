// 
// 
// This file contains a function that computes the normalizing constant of 
//   a TPL with an xmin of one
// 

#include <Rcpp.h>
#define TOLERANCE 1e-64

using namespace Rcpp; 

//[[Rcpp::export]]
NumericVector tplsum(double expo, double rate, IntegerVector xs) { 
  
  NumericVector output(xs.length());
  
  for (int i=0; i<xs.length(); i++) { 
    int x = xs(i);
    double total = 0;
    for (int k=1; k<x; k++) { 
      total += pow(k, -expo) * exp(-k*rate);
    }
    output(i) = total;
  }
  
  return(output);
}

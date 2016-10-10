// 
// 
// This file contains a function that computes the normalizing constant of 
//   a TPL with an xmin of one
// 

#include <Rcpp.h>
#define TOLERANCE 1e-64

using namespace Rcpp; 

//[[Rcpp::export]]
double tplsum(double expo, double rate, 
              int from, int to) { 
  
  double ans = 0; 
  double newval = 0; 
  for (int k=from; k <= to; k++) { 
    newval = pow(k, -expo) * exp(-k * rate);
    ans += newval;
    
  }
  
  return ans;
}

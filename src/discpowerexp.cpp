// 
// 
// This file contains a routing to compute the normalization parameter 
//   of a discrete powerexp distribution
// 

#include <Rcpp.h>

using namespace Rcpp;

#define NUM_TERMS 1000000

//[[Rcpp::export]]
double discpowerexp_norm(double expo, 
                         double rate, 
                         int xmin) { 
  double norm=0;
  
  for ( int x=xmin; x < xmin+NUM_TERMS; x++) { 
    norm += pow(x, -expo)*exp(-rate*x);
  }
  
  return norm; 
}

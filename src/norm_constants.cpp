// 
// 
// This file contains a function that computes the normalizing constant of 
//   a TPL. The function is vectorized. 
// 

#include <Rcpp.h>

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


// This function will carry out the above computation until "infinity", i.e. 
// when the relative change in the sum value goes below a tolerance threshold
// or reaches a maximum number of iterations. The constant returned is used 
// as a normalization term when computing the probabilities of truncated 
// power laws. 
const double TOL = 1e-8; 
const int MAXIT = 1e6L; 
//[[Rcpp::export]]
double tplinfsum(double expo, double rate, int xmin) { 
  
  double current_term = pow(xmin, -expo) * exp(-xmin*rate);
  double total = current_term;
  double rel_change = current_term / total; 
  double it = 0; 
  int k = xmin + 1; 
  
  while ( it < MAXIT && TOL < rel_change ) { 
    current_term = pow(k, - expo) * exp(- k * rate);
    rel_change = current_term / total; 
    total += current_term; 
//     Rcpp::Rcerr << "it : " << it << " ct: " << current_term << " relc: " << 
//       rel_change << " total: " << total << "\n"; 
    it++; 
    k++; 
  }
  
  return(total);
}



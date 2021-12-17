// 
// 
// This file contains a function that computes the normalizing constant of 
//   a TPL. The function is vectorized. 
// 

#include <RcppArmadillo.h>

using namespace arma; 

const int MAXIT = 1e6L; 
const double TOL = 1e-8; 
  

//[[Rcpp::export]]
arma::vec tplsum(double expo, double rate, arma::ivec xs, int xmin) { 
  
  arma::vec output(xs.n_elem);
  
  for (int i=0; i<xs.n_elem; i++) { 
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
//[[Rcpp::export]]
double tplinfsum(double expo, double rate, int xmin) { 
  
  double current_term = pow(xmin, -expo) * exp(-xmin*rate);
  double total = current_term;
  double rel_change = 1.0; 
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

// 
// This function will compute the lerch phi function. It is a simplified 
// version of what is done in the VGAM source code (© Thomas Yee), which was 
// originally written by 
// 
// Sergej V. Aksenov (http://www.geocities.com/saksenov) and 
// Ulrich D. Jentschura (jentschura@physik.tu-dresden.de), 2002.
// 
// 
//[[Rcpp::export]]
long int lerchphi(double z, double s, long int v) { 
  
  // If z is above 1.0, the sum diverges. Return Inf
  if ( z > 1.0 ) { 
    return(R_PosInf); 
  }
  
  // If z *is* 1.0, but s is below one, then the series diverges
  // CASE NOT HANDLED
  
  // If v is below zero, the function is undefined 
  if ( v <= 0.0 ) { 
    return(R_NaN); 
  }
  
  // In other cases, we proceed with computing the sum
  double total = 0; 
  for (int k=1; k<=MAXIT; k++) { 
    double sumterm = pow(z, k) / pow(k + v, s); 
    total+= sumterm; 
    if ( (sumterm/total) < TOL ) { 
      break; 
    }
  }
  
  return(total); 
}


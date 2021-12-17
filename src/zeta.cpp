// 
// 
// This file is a function that helps the computation of zeta_w_xmin
// 

#include <Rcpp.h>

using namespace Rcpp; 

//[[Rcpp::export]]
double sum_all_one_over_k(int from, 
                          int to, double expo) { 
  
  double total = 0;
  for (int k=from; k<to; k++) { 
    total += pow(k,-expo);
  }
  
  return(total); 
}
/*
double sum_all_one_over_k_before(int n, double expo) { 
  
  double total = 0;
  for (int k=1; k<n; k++) { 
    total += pow(k,-expo);
  }
  
  return(total); 
}*/

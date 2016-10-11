// 
// 
// This file contains a function that computes the normalizing constant of 
//   a TPL with an xmin of one
// 

#include <Rcpp.h>
#define xsLERANCE 1e-64

using namespace Rcpp; 

//[[Rcpp::export(".tplsum")]]
NumericVector tplsum(double expo, double rate, 
                     IntegerVector xs) { 
  // This assumes that xs is sorted
  
  NumericVector output(xs.length());
  
  // Init
  double xstal = exp(-rate); // value for k == 1 
  int current_k = 1; 
  
  for (int i=0; i < xs.length(); i++) { 
    // Consider next k xs come
    int next_k = xs(i);
//     Rcout << "new_k:" << next_k << "\n";
    if ( next_k > current_k ) { 
      for (int j=(current_k+1); j <= next_k; j++) { 
//         Rcout << "summing: " << j << "\n";
        xstal += pow(j, -expo) * exp(-j * rate);
      }
    }
    current_k = next_k;
    output(i) = xstal;
  }
  
  return output;
}



// 
// Myfftshift implemented in cpp for speed
// 

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name = ".myfftshift_cpp")]]
ComplexMatrix myfftshift(ComplexMatrix mat) { 
  
  // Mat is square so no need to check for number of columns
  int n = mat.nrow();
  ComplexMatrix newmat(n, n);
  
  int shift = floor(n/2);
  
  for (int i=0; i<n; i++) { 
    for (int j=0; j<n; j++) { 
      int a = (i + shift) % n;
      int b = (j + shift) % n;
      
      // if ( a == 0 ) a = n;
      // if ( b == 0 ) b = n;
      
      newmat(a, b) = mat(i, j);
    }
  }
  
  return newmat;
}

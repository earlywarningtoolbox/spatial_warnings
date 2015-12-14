// 
// Helper function for rspectrum
// 

#include <Rcpp.h>
using namespace Rcpp;

// WARNING: THIS FUNCTION CAN MODIFY ASPECTR2d IN-PLACE !!!
//[[Rcpp::export]]
NumericMatrix normalize(NumericMatrix aspectr2D, 
                        NumericMatrix dists,
                        int n0x,
                        int n0y) { 
  
  // Get sizes of matrix
  int nr = aspectr2D.nrow();
  int nc = aspectr2D.ncol();
  
  // Get inside square matrix
  int mi = 1;
  int ma = n0x < n0y ? n0x : n0y;
  
  // Normalization factor
  double norm_factor = 0;
  for (int i=0; i<nr; i++) { 
    for (int j=0; j<nc; j++) { 
      if ( dists(i,j) >= mi && dists(i,j) <= ma ) { 
        norm_factor += aspectr2D(i, j);
      }
    }
  }
//   Rcout << "sig2:" << norm_factor << " - ma: " << ma << std::endl;
  
  // Normalize 
  for (int i=0; i<nr; i++) { 
    for (int j=0; j<nc; j++) { 
      aspectr2D(i,j) = aspectr2D(i,j) / norm_factor;
    }
  }
  return aspectr2D;
}

//[[Rcpp::export]]
NumericMatrix get_distances(int nr, 
                            int nc, 
                            int n0x, 
                            int n0y) { 
  
  NumericMatrix output(nr, nc); 
  
  for (int i=0; i<nr; i++) { 
    for (int j=0; j<nc; j++) { 
      output(i, j) = sqrt( pow((double)(i+1 - n0x), 2) + 
                           pow((double)(j+1 - n0y), 2) );
    }
  }
  
  return output;
}

//[[Rcpp::export]] 
NumericVector get_rspectr(NumericVector ray, 
                          double step, 
                          NumericMatrix dists, 
                          NumericMatrix aspectr2D) { 
  
  NumericVector rspectr = NumericVector(ray.length());
  
  int nr = dists.nrow();
  int nc = dists.ncol(); 
  
  // For all lengths
  for (int l=0; l<ray.length(); l++) { 
    double r = ray(l);
    
    // Go through the distances matrix and make a sum of the relevant ones
    int total_inmask = 0;
    for (int i=0; i<nr; i++) { 
      for (int j=0; j<nc; j++) { 
        
        if ( dists(i, j) >= r - step/2 & 
               dists(i, j) < r + step/2 ) { 
          
          rspectr(l) += aspectr2D(i, j);
          total_inmask++;
        }
        
      }
    }
    rspectr(l) = rspectr(l) / total_inmask;
  }
  
  return rspectr;
}

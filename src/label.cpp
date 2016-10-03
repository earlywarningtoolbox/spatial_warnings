// 
// A function that does the patch labelling
// 

#include <Rcpp.h>
#include "./headers.h"

using namespace Rcpp;

// Default value for non-patch cells
#define DEFAULT_VALUE NA_INTEGER

// [[Rcpp::export(name = ".label")]]
IntegerMatrix label(IntegerMatrix mat, 
                    IntegerMatrix nbmask,
                    bool wrap) { 
  
  int W = mat.ncol();
  int H = mat.nrow();
  
  IntegerMatrix output(H,W);
  LogicalMatrix is_marked(H,W);
  int patch_number = 1;
  
  for (int i=0; i<W; i++) { 
    for (int j=0; j<W; j++) { 
      // We consider the cell (i,j).
      
      // Default value is NA if not in a patch
      if ( ! mat(i,j) > 0) { 
        output(i,j) = DEFAULT_VALUE;
      
      // If it is within a patch and not marked already 
      } else if ( ! is_marked(i,j) ) { 
      
        // We flood fill the patch
        IntegerVector X = IntegerVector::create(i,j);
        flood_fill(mat, is_marked, output, nbmask, X, patch_number, wrap);
        patch_number++;
        
      } 
    }
  }
  
  return(output);
}

// We use an implicit stack here which is not very efficient but easier on my 
// brain and probably enough for our use.
void flood_fill(const IntegerMatrix &mat, 
                LogicalMatrix &is_marked,
                IntegerMatrix &output,
                IntegerMatrix nbmask,
                IntegerVector X,
                int fillcol,
                bool wrap) { 
  
  int i = X(0);
  int j = X(1);
  
  // We paint the pixel in our fill color
  if (mat(i,j) > 0) { 
    is_marked(i,j) = 1;
    output(i,j) = fillcol;
    
    // We consider its neighbors
    IntegerMatrix nb = get_nb_coords(mat, X, nbmask, wrap);
    
    for (int n=0; n<nb.nrow(); n++) { 
      // We flood fill them if they are vegetated and not-marked
      if ( mat(nb(n,0),nb(n,1)) > 0 && ! is_marked(nb(n,0), nb(n,1)) ) {
        IntegerVector X_new = IntegerVector::create(nb(n,0), nb(n,1));
        flood_fill(mat, is_marked, output, nbmask, X_new, fillcol, wrap);
      }
    }
  }
  
}

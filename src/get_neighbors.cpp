// 
// 
// A function that returns the coordinates of neighboring cells in a matrix
// 
// NOTE: We export it as a hidden function .get_nb_coords as we have some 
//   indexing adjustments to do when calling from R.
// 

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export(name =".get_nb_coords")]]
IntegerMatrix get_nb_coords(IntegerMatrix mat, 
                            IntegerVector X,
                            IntegerMatrix nbmask, 
                            bool wrap) { 
  
  // We insert the result in a nb*3 matrix for the cell's i, j and value
  int nbmax = sum(nbmask); // total nb of neighbors considered
  
  IntegerMatrix neighbors_xy(nbmax, 2);
  
  int W = mat.ncol();
  int H = mat.nrow();
  
  int curnb = 0;
  for (int nbi=0; nbi<nbmask.nrow(); nbi++) { 
    for (int nbj=0; nbj<nbmask.ncol(); nbj++) { 
      // If the neighbor is to be considered, record the necesary shifts
      if ( nbmask(nbi, nbj) > 0) { 
        int shift_x = nbi - ceil( (nbmask.nrow()-1)/2 );
        int shift_y = nbj - ceil( (nbmask.ncol()-1)/2 );
        
        // Actual coordinates of the neighbor
        int nb_x = (X(0) + shift_x);
        int nb_y = (X(1) + shift_y);
        
        // Does the neighbor fall outside the matrix ? 
        bool is_out = nb_x < 0 | nb_x >= W | nb_y < 0 | nb_y >= H;
        
        // It is out and we don't wrap around: do not count this neighbor
        if ( !wrap && is_out ) { 
          // Nothing
        } else { // The neighbors falls within the field (or we wrap around)
          neighbors_xy(curnb, 0) = (nb_x + W) % W;
          neighbors_xy(curnb, 1) = (nb_y + H) % H;
          curnb++;
        }
      }
    }
  }
  
  // We ignored some neighbors because they were out of the field: we might 
  // need to shorten the output matrix
  if (!wrap && curnb < nbmax) { 
    IntegerMatrix tmp(curnb, 2);
    for (int i=0; i<curnb; i++) { 
      tmp(i,0) = neighbors_xy(i,0);
      tmp(i,1) = neighbors_xy(i,1);
    }
    neighbors_xy = tmp;
  }
  
  colnames(neighbors_xy) = CharacterVector::create("x","y");
  return neighbors_xy; 
}


// Get the neighbors' values
// [[Rcpp::export(name =".get_nb_values")]]
IntegerVector get_nb_values(IntegerMatrix mat, 
                            IntegerVector X,
                            IntegerMatrix nbmask, 
                            bool wrap) { 
  
  IntegerMatrix neighbors_xy = get_nb_coords(mat, X, nbmask, wrap);
  
  IntegerVector vals(neighbors_xy.nrow());
  for (int i=0; i<vals.length(); i++) { 
    vals(i) = mat(neighbors_xy(i, 0), neighbors_xy(i, 1));
  }
  
  return vals;
}

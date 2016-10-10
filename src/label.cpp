// 
// A function that does the patch labelling
// 

#include <Rcpp.h>
#include <queue>
#include <utility>
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
  std::vector<int> patchsizes;
  int new_size;
  
  for (int i=0; i<W; i++) { 
    for (int j=0; j<H; j++) { 
      // We consider the cell (i,j).
      
      // Default value is NA if not in a patch
      if ( ! mat(i,j) > 0) { 
        output(i,j) = DEFAULT_VALUE;
        is_marked(i,j) = 1;
      }
      
      // If it is within a patch and not marked already 
      if ( !is_marked(i,j) ) { 
        // We flood fill the patch
        std::pair<int,int> xy = std::make_pair(i,j);
        new_size = flood_fill(mat, is_marked, output, nbmask, 
                                              xy, patch_number, wrap);
        patchsizes.push_back(new_size);
        patch_number++;
      }
      output(i,j);
    }
  }
  
  output.attr("psd") = patchsizes;
  return(output);
}


// This flood fill is implemented with a queue
int flood_fill(const IntegerMatrix &mat, 
                LogicalMatrix &is_marked,
                IntegerMatrix &output,
                IntegerMatrix nbmask,
                std::pair<int, int> xy,
                int fillcol,
                bool wrap) { 
  
  // Create empty queue
  std::queue <std::pair<int, int> > to_fill;
  
  // Keep a count of the number of cell counted
  int total_marked = 0;
  
  // Add our cell to the queue
  to_fill.push(xy);
  is_marked(xy.first, xy.second) = 1; // The cell has been pushed to the queue
  total_marked++;
  
  while ( !to_fill.empty() ) { 
    xy = to_fill.front();
    to_fill.pop(); 
    int i = xy.first;
    int j = xy.second;
    
    // We paint the pixel in our fill color (it always starts with vegetation)
    output(i,j) = fillcol;
    
    // We consider its neighbors
    IntegerMatrix nb = get_nb_coords(mat, xy, nbmask, wrap);
    
    // We add the neighbors to the queue if needed
    for (int n=0; n<nb.nrow(); n++) { 
      int newx = nb(n, 0);
      int newy = nb(n, 1);
      
      if ( !is_marked(newx, newy) && (mat(newx, newy) > 0) ) { 
        // We mark this cell to know it has been put in the queue
        is_marked(newx, newy) = 1;
        total_marked++;
        std::pair<int,int> xynew = std::make_pair(newx, newy);
        to_fill.push(xynew);
      }
    }
  }
  return total_marked;
}

// A function that returns the coordinates of neighboring cells in a matrix,
//   taking into account the wraparound
IntegerMatrix get_nb_coords(IntegerMatrix mat, 
                            std::pair<int,int> X,
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
        int nb_x = (X.first + shift_x);
        int nb_y = (X.second + shift_y);
        
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
  
  return neighbors_xy; 
}


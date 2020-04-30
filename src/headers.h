
// 
// These headers are here so that Rcpp functions can call each other
// 

#ifndef HEADERS_INCLUDED
#define HEADERS_INCLUDED

using namespace Rcpp;

std::queue <std::pair<int, int>> get_nb_coords(const int W, // width
                                               const int H, // height
                                               const std::pair<int,int> X,
                                               const IntegerMatrix& nbmask, 
                                               const bool wrap); 

IntegerVector flood_fill(const IntegerMatrix &mat, 
                               LogicalMatrix &is_marked,
                               IntegerMatrix &output,
                               IntegerMatrix nbmask,
                               std::pair<int, int> xy,
                               int fillcol,
                               bool wrap);

NumericMatrix coarse_grain(NumericMatrix mat, 
                                 int subsize);

#endif

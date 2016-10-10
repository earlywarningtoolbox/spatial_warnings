
// 
// These headers are here so that Rcpp functions can call each other
// 

#ifndef HEADERS_INCLUDED
#define HEADERS_INCLUDED

using namespace Rcpp;; 

IntegerMatrix get_nb_coords(IntegerMatrix mat, 
                            std::pair<int,int> X,
                            IntegerMatrix nbmask, 
                            bool wrap);

int flood_fill(const IntegerMatrix &mat, 
                LogicalMatrix &is_marked,
                IntegerMatrix &output,
                IntegerMatrix nbmask,
                std::pair<int, int> xy,
                int fillcol,
                bool wrap);

Rcpp::NumericMatrix coarse_grain(NumericMatrix mat, 
                                 int subsize);

#endif

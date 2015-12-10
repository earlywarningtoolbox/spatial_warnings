
// 
// These headers are here so that Rcpp functions can call each other
// 

Rcpp::IntegerMatrix get_nb_coords(Rcpp::IntegerMatrix mat, 
                                  Rcpp::IntegerVector X,
                                  Rcpp::IntegerMatrix nbmask, 
                                  bool wrap);

Rcpp::IntegerVector get_nb_values(Rcpp::IntegerMatrix mat, 
                                  Rcpp::IntegerVector X,
                                  Rcpp::IntegerMatrix nbmask, 
                                  bool wrap);

Rcpp::IntegerMatrix label_patches(Rcpp::IntegerMatrix mat, 
                                  Rcpp::IntegerMatrix nbmask,
                                  bool wrap);

void flood_fill(const Rcpp::IntegerMatrix &mat, 
                Rcpp::LogicalMatrix &is_marked,
                Rcpp::IntegerMatrix &output,
                Rcpp::IntegerMatrix nbmask,
                Rcpp::IntegerVector X,
                int fillcol,
                bool wrap);

Rcpp::ComplexMatrix myfftshift(Rcpp::ComplexMatrix mat);

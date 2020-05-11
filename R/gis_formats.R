# 
# This file defines a generic function to convert matrices. spatialwarningsGis
# will add methods to this generic function so that foreign gis formats can be 
# directly used in spatialwarnings. 
# 

convert_to_matrix <- function(object, ...) { 
  UseMethod("convert_to_matrix")
}

convert_to_matrix.matrix <- function(object, ...) { 
  object
}

convert_to_matrix.list <- function(object, ...) { 
  lapply(object, convert_to_matrix) 
}

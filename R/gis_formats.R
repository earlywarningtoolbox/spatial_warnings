# 
# This file contains functions to handle foreign gis formats so they can be 
# used in spatialwarnings
# 

convert_to_matrix <- function(object) { 
  
  if ( inherits(object, "list") ) { 
    return( lapply(object, convert_to_matrix) )
  }
  
  if ( is.matrix(object) ) { 
    return(object)
  }
  
  # If it's not a matrix, then we start calling the appropriate conversion 
  # method. 
  if ( inherits(object, "RasterLayer") ) { 
    if ( requireNamespace("raster", quietly = TRUE) ) { 
      return( raster::as.matrix(object) )
    } else { 
      stop("spatialwarnings requires the raster package to process raster ", 
           "objects")
    }
  }
  
  if ( inherits(object, "RasterBrick") || 
       inherits(object, "RasterStack") ) { 
    stop("spatialwarnings cannot use RasterBrick/RasterStack objects. You ", 
         "must extract single layers from these objects first, then ", 
         "use the package")
  }
  
  # Ultimately, try to use the search path default method to convert it 
  return( as.matrix(object) )
  
}

# 
# 
# This file is a wrapper around the .coarse_grain_unsafe routine to catch 
#   the case when subsize = 0 that crashes R otherwise. 
# 

#@export
coarse_grain <- function(mat, subsize) { 
  
  if ( subsize < 1 ) { 
    warning('Cannot coarse-grain a matrix with a subsize argument under 1, ', 
            'returning the matrix unchanged')
    return(mat)
  }
  
  .coarse_grain_unsafe(mat, subsize)
  
}
  

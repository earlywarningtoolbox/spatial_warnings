# 
# 

# Make an indicator function with coarse_graining 
with_coarse_graining <- function(indicf, subsize) { 
  function(mat) { 
    indicf( coarse_grain(mat, subsize) )
  }
}

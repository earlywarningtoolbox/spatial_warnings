# 
# 

# Make an indicator function with coarse_graining 
with_coarse_graining <- function(indicf, subsize) { 
  function(mat) { 
    indicf( coarse_grain(mat, subsize) )
  }
}

# Function that transforms a set of values into stars for display
pval_stars <- function(value) { 
  
  if ( length(value) > 1 ) { 
    return( sapply(value, pval_stars) )
  }
  
  if (value < 0.001) { 
    return('*** ' )
  } else if (value < 0.01) { 
    return('**  '  )
  } else if (value < 0.05) { 
    return('*   '   )
  } else if (value < 0.1) { 
    return('.   '   )
  } else { 
    return('    ')
  }
}

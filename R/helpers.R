# 
# 

# Make an indicator function with coarse_graining 
with_coarse_graining <- function(indicf, subsize) { 
  function(mat) { 
    indicf( coarse_grain(mat, subsize) )
  }
}

# Function that transforms a set of values into stars for display
pval_stars <- function(value, NA_ret = NA) { 
  
  if ( length(value) > 1 ) { 
    stars <- sapply(value, pval_stars, NA_ret = NA_ret) 
    attributes(stars) <- attributes(value)
    return(stars)
  }
  
  if ( is.na(value) ) { 
    return(NA_ret) 
  }
  
  if (value < 0.001) { 
    return('***' )
  } else if (value < 0.01) { 
    return('** '  )
  } else if (value < 0.05) { 
    return('*  '   )
  } else if (value < 0.1) { 
    return('.  '   )
  } else { 
    return('   ')
  }
}


# Print a line with matrix size information 
display_size_info <- function(x, ...) { 
  UseMethod('display_size_info')
}

# ... for spews_result class
display_size_info.spews_result <- function(x) { 
  sizes <- sapply(x, function(x) dim(x[["orig_data"]]))
  sizes <- apply(sizes, 1, function(X) length(unique(X)) == 1)
  has_different_sizes <- ! any(sizes)
  if (has_different_sizes) { 
    size_text_report <- "variable sizes"
  } else { 
    size_text_report <- paste0("size: ", nrow(x[[1]][["orig_data"]]), 'x', 
                               ncol(x[[1]][["orig_data"]]))
  } 
  cat(' ', 
      length(x), ' ', 
      ifelse(length(x)>1, 'matrices', 'matrix'), ' ',
      "(", size_text_report,')\n', sep = '')
}

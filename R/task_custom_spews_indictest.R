# 
# This file contains a function to test significance of custom indicators 
# 
# WARNING: indictest will always transform original matrix to numeric 
# internally, be careful when your function works with logical data. 
# 
#' @rdname create_indicator
#' 
#' @param nperm The number of replicates to use to compute use in the 
#'   null distribution
#' 
#' @export
indictest.simple_sews <- function(x, nperm = 999, ...) { 
  NextMethod('indictest')
}
#'@export
indictest.simple_sews_single <- function(x, nperm = 999, ...) { 
  
  # We do not support low numbers of replicates
  if ( nperm < 3 ) { 
    stop('The number of null replicates should be above 3 to ', 
         'assess significance')
  }
  
  new_indicf <- function(mat) { 
    do.call(x[['indicf']], c(mat = list(mat), x[['fun.args']]))
  }
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(x[["orig_data"]],
                                             nreplicates = nperm, 
                                             indicf = new_indicf)
  
  # Format result
  results <- c(null_values, x["fun.name"], list(nperm = nperm))
  class(results) <- c('simple_sews_test_single', 'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  
  return(results)
}
#'@export
indictest.simple_sews_list <- function(x, nperm = 999, ...) { 
  
  results <- parallel::mclapply(x, indictest.simple_sews_single, 
                                nperm, ...)
  
  # Add replicate column with correct replicate number
  for ( nb in seq_along(results) ) { 
    results[[nb]][['replicate']] <- nb
  }
  
  class(results) <- c('simple_sews_test_list', 'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  return(results)
}

# Safe version of as.data.frame.list that removes NULL elements present 
# in a list prior to converting to data.frame
as.data.frame.list_safe <- function(l) { 
  isNULL <- unlist(lapply(l, is.null))
  as.data.frame.list( l[!isNULL] )
}

#'@method as.data.frame simple_sews_test_single
#'@export
as.data.frame.simple_sews_test_single <- function(x, ...) { 
  
  # We need to explicitely add a `replicate` column because it will 
  # be used by funs down the stream. 
  data.frame(replicate = 1, as.data.frame.list_safe(x))
}
#'@method as.data.frame simple_sews_test_list
#'@export
as.data.frame.simple_sews_test_list <- function(x, ...) { 
  tab <- ldply(x, as.data.frame.list_safe)
  # Reorder cols
  tab <- data.frame(replicate = tab[ ,'replicate'], 
                    tab[ ! names(tab) %in% 'replicate'])
  tab
}


#'@method as.data.frame simple_sews_test_list
#'@export
summary.simple_sews_test_single <- function(object, ...) { 
  summary.simple_sews_test_list( list(object) )
}
#'@export
summary.simple_sews_test_list <- function(object, ...) { 
  
  tab <- as.data.frame(object)
  # Get some info about the computation
  indicname <- attr(object, "indicname")
  indicname <- ifelse(is.null(indicname), "Unknown indicator", indicname)

  nperm    <- tab[1, "nperm"]
  
  # Format pvalues 
  tab[ ,'stars'] <- pval_stars(tab[ ,'pval'])
  
  tab <- tab[ ,c('replicate', 'value', 'pval', 'stars')]
  names(tab) <- c('Mat. #', "Value", 'P>null', "")
  
  cat('Spatial Early-Warning:', indicname, '\n') 
  cat('\n')
  print.data.frame(tab, row.names = FALSE, digits = DIGITS)
  cat('\n')
  cat(' Significance tested against', nperm, 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n')
  cat('\n')
  
  invisible(tab)
}



#'@export
print.simple_sews_test_single <- function(x, ...) { 
  summary.simple_sews_test_single(x, ...)
}
#'@export
print.simple_sews_test_list <- function(x, ...) { 
  summary.simple_sews_test_list(x, ...)
}



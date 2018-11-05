# 
# Methods for a simple indicator, i.e. an indicator that produces a single 
# value for each matrix. 
# 



# as.df methods
# ---------------
#'@method as.data.frame simple_sews_single
#'@export
as.data.frame.simple_sews_single <- function(x, ...) { 
  as.data.frame.simple_sews_list( list(x) )
}
#'@method as.data.frame simple_sews_list
#'@export
as.data.frame.simple_sews_list <- function(x, ...) { 
  output <- Map(function(n, o) data.frame(replicate = n, value = o[['value']]), 
               seq_along(x), x)
  output <- do.call(rbind, output)
  output
}



# Print methods
# ---------------
#'@method print simple_sews_single
#'@export
print.simple_sews_single <- function(x, ...) { 
  print.simple_sews_list(list(x), ...)
}
#'@method print simple_sews_list
#'@export
print.simple_sews_list <- function(x, ...) { 
  summary.simple_sews_list(x, ...)
}



# Summary methods
# ---------------
#'@method summary simple_sews_single
#'@export
summary.simple_sews_single <- function(object, ...) { 
  summary.simple_sews_list( list(object) )
}
#'@method summary simple_sews_list
#'@export
summary.simple_sews_list <- function(object, 
                                     indicname = attr(object, "indicname"), 
                                     ...) { 
  
  if ( is.null(indicname) ) { 
    indicname <- ""
  }
  
  cat('Spatial Early-Warning:', indicname, '\n') 
  cat('\n')
  display_size_info(object)
  cat('\n')
  
  # Format output table
  output <- as.data.frame(object)[ ,c('replicate', 'value')]
  names(output) <- c('Mat. #', indicname)
  
  print.data.frame(output, row.names = FALSE, digits = DIGITS)
  cat('\n')
  cat('Use as.data.frame() to retrieve values in a convenient form\n')
  
  invisible(output)
}


# Plot methods 
# ------------
#' @rdname create_indicator
#' 
#' @param x A \code{custom_sews} object (as provided by the 
#'   custom indicator function created by \code{create_indicator}). 
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. If \code{NULL} then the values are plotted sequentially 
#'   in their original order. 
#' 
#' @param ... Ignored
#' 
#'@method plot simple_sews_list
#'@export
plot.simple_sews_list <- function(x, along = NULL, ...) { 
  plot.simple_sews_test_list(x, along = along, display_null = FALSE)
}

#'@method plot simple_sews_single
#'@export
plot.simple_sews_single <- function(x, ...) { 
  stop('I cannot plot a trend with only one value !')  
}



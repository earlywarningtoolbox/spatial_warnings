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
as.data.frame.simple_sews_list <- function(x, wide = FALSE, ...) { 
  
  # Find or create the indicator names
  indicnames <- ifNULLthen(names(x[[1]][['value']]), 
                           paste0("indic_", seq_along(x[[1]][['value']])))
  
  if ( wide ) { 
    output <- Map(function(n, o) { 
        a <- as.data.frame(matrix(o[['value']], nrow = 1))
        names(a) <- indicnames
        data.frame(replicate = n, a)
      }, seq_along(x), x)
  } else { 
    output <- Map(function(n, o) { 
        data.frame(replicate = n, indic = indicnames, value = o[['value']])
      }, seq_along(x), x)
  }
  output <- do.call(rbind, output)
  row.names(output) <- NULL
  output
}



# Print methods
# ---------------
#'@method print simple_sews_single
#'@export
print.simple_sews_single <- function(x, ...) { 
  x.list <- list(x)
  attr(x.list, "indicname") <- attr(x, "indicname")
  summary.simple_sews_list(x.list, ...)
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
  object.list <- list(object)
  attr(object.list, "indicname") <- attr(object, "indicname")
  summary.simple_sews_list( object.list )
}
#'@method summary simple_sews_list
#'@export
summary.simple_sews_list <- function(object, 
                                     indicname = attr(object, "indicname"), 
                                     ...) { 
  
  if ( is.null(indicname) ) { 
    indicname <- "unknown indicator(s)"
  }
  
  cat('Spatial Early-Warning:', indicname, '\n') 
  cat('\n')
  display_size_info(object)
  cat('\n')
  
  # Format output table
  output <- as.data.frame(object, wide = TRUE)
  names(output)[1] <- c('Mat. #')
  
  print.data.frame(output, row.names = FALSE, digits = DIGITS)
  cat('\n')
  cat('Use as.data.frame() to retrieve values in a convenient form\n')
  
  invisible(output)
}


# Plot methods 
# ------------
# 
#' @title Spatial early-warning signals: display of trends
#' 
#' @param x A \code{simple_sews} object (as provided by **_sews functions, such 
#'   as \code{generic_sews()} or \code{kbdm_sews()}). 
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. If \code{NULL} then the values are plotted sequentially 
#'   in their original order. 
#' 
#' @details Note that the produced plot is adjusted depending on whether 
#'   \code{along} is numeric or not. 
#' @param what The trendline to be displayed. Defaults to the indicator's 
#'   values ("value") but other metrics can be displayed. Correct values are 
#'   "value", "pval" or "z_score".
#' 
#' @param display_null Chooses whether a grey ribbon should be added to reflect
#'   the null distribution. Note that it can not be displayed when the trend 
#'   line reflects something else than the indicator values (when \code{what} 
#'   is not set to "value").
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



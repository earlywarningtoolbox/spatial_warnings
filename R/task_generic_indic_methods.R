# TODO: summary methods for single and list
# 
# This file contains common methods (plot/print/summary/as.data.frame) used for 
#   generic indicators. 
# 

# ----------------------------
# PLOT METHODS
# ----------------------------
# 
#' @title Generic spatial warning signals: plotting function
#' 
#' @description Plot function for generic early warning signals
#' 
#' @param obj A \code{generic_spews} object (as provided by the 
#'   \code{generic_spews} function). 
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. 
#' 
#' @details A ggplot object (usually displayed immediatelly when called at the 
#'   prompt). 
#' 
#' @details Since this function returns a ggplot object, it can be later 
#'   modified to add other graphical elements (e.g. axis names or annotations). 
#' 
#' @seealso \code{\link{generic_spews}}, 
#'   \code{\link{plot.generic_spews_test}}
# 
#'@export
plot.generic_spews <- function(obj, along = NULL) { 
  if ( 'generic_spews_single' %in% class(obj) ) { 
    stop('I cannot plot a trend with only one value !')
  }
  
  new_data <- indictest.generic_spews_list(obj, null_replicates = 0)
  plot.generic_spews_test(new_data, along, display_null = FALSE)
}

# As print methods

#'@export
print.generic_spews <- function(obj, ...) { 
  cat('Generic Spatial Early-Warnings results\n') 
  cat('\n')
  
  NextMethod("print", obj)
}

#'@export
print.generic_spews_single <- function(obj, ...) { 
  output <- indictest.generic_spews_single(obj, null_replicates = 0) 
  row.names(output) <- NULL
  print.data.frame(output)
}

#'@export
print.generic_spews_list <- function(obj, ...) { 
  output <- indictest.generic_spews_list(obj, null_replicates = 0) 
  row.names(output) <- NULL
  print.data.frame(output)
}

# As data.frame methods

#'@export
as.data.frame.generic_spews_list <- function(obj) { 
  
  df <- plyr::ldply(obj, function(x) { as.data.frame(x[['results']]) })
  df[ ,'replicate'] <- seq.int(length(obj))
  
  # Extract and reorder the data.frame
  df <- df[ ,c('replicate', 'mean', 'moran', 'skewness', 'variance')]
  tidyr::gather_(df, 'indicator', 'value', 
                 c('mean', 'moran', 'skewness', 'variance'))
}

#'@export
as.data.frame.generic_spews_single <- function(obj) { 
  as.data.frame.generic_spews_list(list(obj))
}


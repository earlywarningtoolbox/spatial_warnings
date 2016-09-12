# TODO: summary methods for single and list
# 
# This file contains common methods (plot/print/summary/as.data.frame) used for 
#   generic indicators objects (without significance assessment). 
# 





# Plot methods
# --------------------------------------------------



# NOTE: we do not document these args as they are already included by another
#   function in the generic_spews doc file
#  
#' @rdname generic_spews
#' 
#' @method plot generic_spews
#' @export
plot.generic_spews <- function(obj, along = NULL) { 
  if ( 'generic_spews_single' %in% class(obj) ) { 
    stop('I cannot plot a trend with only one value !')
  }
  
  new_data <- as.data.frame(obj)
  plot.generic_spews_test(new_data, along, display_null = FALSE)
}




# Print methods
# --------------------------------------------------
  
#'@export
print.generic_spews <- function(obj, ...) { 
  output <- as.data.frame(obj) 
  row.names(output) <- NULL
  print.data.frame(output)
}




# Summary methods
# --------------------------------------------------

# This function works for both list and single object
#'@export
summary.generic_spews <- function(obj) { 
  
  cat('Generic Spatial Early-Warnings\n') 
  cat('\n')
  
  # Format output table
  output <- as.data.frame(obj)
  output <- reshape2::dcast(output,  replicate ~ indicator, value.var = 'value')
  names(output) <- c('Replicate', 'Mean', 'Moran\'s I', 'Skewness', 'Variance')
  
  print.data.frame(output, row.names = FALSE)
}



# As data.frame methods
# --------------------------------------------------

#'@export
as.data.frame.generic_spews_list <- function(x, ...) { 
  
  df <- plyr::ldply(x, function(x) { as.data.frame(x[['results']]) })
  df[ ,'replicate'] <- seq.int(length(x))
  
  # Extract and reorder the data.frame
  df <- df[ ,c('replicate', 'mean', 'moran', 'skewness', 'variance')]
  tidyr::gather_(df, 'indicator', 'value', 
                 c('mean', 'moran', 'skewness', 'variance'))
}

#'@export
as.data.frame.generic_spews_single <- function(x, ...) { 
  as.data.frame.generic_spews_list(list(x))
}


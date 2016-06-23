# 
# 
# This file contains common methods (print/summary/as.data.frame) used for 
#   generic indicators. 
# 

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


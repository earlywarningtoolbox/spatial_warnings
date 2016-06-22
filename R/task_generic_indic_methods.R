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


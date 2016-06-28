# 
# 
# This file contains methods related to objects of the patchdistr task
# 


#'@export
as.data.frame.patchdistr_spews_single <- function(obj) { 
  data.frame(replicate   = 1, 
             model       = names(obj[['models']]),
             likelihoods = unlist(obj[['likelihoods']]), 
             is_best     = names(obj[['models']]) == obj[['best']])
}

#'@export
as.data.frame.patchdistr_spews_list <- function(obj) { 
  # Format data
  results <- lapply(obj, as.data.frame.patchdistr_spews_single)
  results <- Map(function(n, df) { df[ ,'replicate'] <- n; df }, 
                 seq.int(length(results)), results)
  
  # Bind it altogether and return
  do.call(rbind, results)
}


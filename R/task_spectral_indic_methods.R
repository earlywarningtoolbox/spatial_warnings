# 
# 
# This file contains common methods to handle spectral_sews objects
# 



# As data.frame methods
# --------------------------------------------------

#'@export
as.data.frame.spectral_sews_list <- function(x, ...) { 
  
  # Compute a distribution of null values for SDR
  results <- plyr::llply(x, as.data.frame.spectral_sews_single, ...)
  
  # Add a replicate column with replicate number
  results <- Map(function(x, df) { df[ ,'replicate'] <- x; df }, 
                 seq.int(length(results)), results)
  
  # Bind all of it in a single df
  results <- do.call(rbind, results)
  
  return(results)
}

#'@export
as.data.frame.spectral_sews_single <- function(x, ...) { 
  
  with(x, 
    rbind.fill(data.frame(replicate = 1, 
                          type = 'sdr', 
                          value = results[['sdr']]), 
               data.frame(replicate = 1, 
                         type  = 'rspectrum', 
                         dist  = results[['spectrum']][ ,'dist'],
                         value = results[['spectrum']][ ,'rspec'])) 
  )
  
}




# Print methods
# --------------------------------------------------

#'@export
print.spectral_sews <- function(x, ...) { 
  cat('Spectral Spatial Early-Warnings results\n') 
  cat('\n')
  
  print.data.frame( as.data.frame(x) )
  
  cat('\n')
  cat('Use as.data.frame() to retrieve spectrum and SDR values in a convenient form\n')
  
  invisible(x)
}




# Summary methods
# --------------------------------------------------

#'@export
summary.spectral_sews_list <- function(object, ...) { 
  cat('Spectral Spatial Early-Warnings results\n') 
  cat('\n')
  
  # Compute correct size to display
  display_size_info(object) 
  cat("\n")
  
  # Show only SDR and print as data frame
  x2 <- as.data.frame(object)
  x2 <- x2[x2[ ,"type"] == "sdr", c('replicate', 'value')]
  names(x2) <- c('Matrix #', 'SDR Value')
  
  print.data.frame(x2, row.names = FALSE)
  
  cat('\n')
  cat('Use as.data.frame() to retrieve r-spectrum and SDR values in a convenient form\n')

}

#'@export
summary.spectral_sews_single <- function(object, ...) { 
  summary.spectral_sews_list(list(object))
}

# 
# 
# This file contains common methods to handle spectral_spews objects
# 

# 
# as.data.frame methods for spectral_spews objects
#'@export
as.data.frame.spectral_spews_list <- function(x, ...) { 
  
  # Compute a distribution of null values for SDR
  results <- plyr::llply(x, as.data.frame.spectral_spews_single, ...)
  
  # Add a replicate column with replicate number
  results <- Map(function(x, df) { df[ ,'replicate'] <- x; df }, 
                 seq.int(length(results)), results)
  
  # Bind all of it in a single df
  results <- do.call(rbind, results)
  
  return(results)
}

#'@export
as.data.frame.spectral_spews_single <- function(x, ...) { 
  
  with(x, 
    plyr::rbind.fill(data.frame(replicate = 1, 
                                type = 'sdr', 
                                value = results[['sdr']]), 
                     data.frame(replicate = 1, 
                                type  = 'rspectrum', 
                                dist  = results[['spectrum']][ ,'dist'],
                                value = results[['spectrum']][ ,'rspec'])) 
  )
  
}


# Print methods for spectral_spews objects
# 
#'@export
print.spectral_spews_list <- function(x, ...) { 
  cat('Spectral Spatial Early-Warnings results\n') 
  cat('\n')
  
  print.data.frame( as.data.frame(x) )
  
  cat('\n')
  cat('Use as.data.frame() to retrieve spectrum and SDR values in a convenient form\n')
  
  invisible(x)
}

#'@export
print.spectral_spews_single <- function(x, ...) { 
  print.spectral_spews_list(list(x))
  invisible(x)
}


# Summary methods for spectral_spews objects
#'@export
summary.spectral_spews_list <- function(x, ...) { 
  cat('Spectral Spatial Early-Warnings results\n') 
  cat('\n')
  
  # Compute correct size to display
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
  cat('\n')
  
  # Show only SDR and print as data frame
  x2 <- as.data.frame(x)
  x2 <- subset(x2, type == 'sdr')[ ,c('replicate', 'value')]
  names(x2) <- c('Replicate #', 'SDR Value')
  
  print.data.frame(x2, row.names = FALSE)
  
  cat('\n')
  cat('Use as.data.frame() to retrieve r-spectrum and SDR values in a convenient form\n')

}

#'@export
summary.spectral_spews_single <- function(x, ...) { 
  summary.spectral_spews_list(list(x))
}


#'@export
print.spectral_spews_test <- function(x) { 
  cat('Spectral Spatial Early-Warnings results\n') 
  cat('\n')
  
  # Show only SDR and print as data frame
  x2 <- as.data.frame(x)
  x2 <- subset(x2, type == 'sdr')[ ,c('replicate', 'value', 'pval')]
  x2<- data.frame(x2, stars = pval_stars(x2[ ,'pval']))
  names(x2) <- c('Replicate #', 'SDR Value', 'P>null', '   ')
  print.data.frame(x2, row.names = FALSE)
  
  cat('\n')
  cat(' Significance tested against', attr(x, 'nreplicates'), 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", '\n')
  cat('\n')

  invisible(x)
}


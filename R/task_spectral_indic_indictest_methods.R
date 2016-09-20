# 
# 
# File that contains methods for spectral_indictest objects
# 


# Note: we reuse the print method for spectral spews that works well
#' @method print spectral_spews_test
#'@export 
print.spectral_spews_test <- function(x, ...) { 
  print.spectral_spews(x, ...)
}

# Note: spectral_spews_test objects are already data.frames. We explicitely 
#   assign the method here in case we want to modify it later
#' @method as.data.frame spectral_spews_test
#' @export
as.data.frame.spectral_spews_test <- as.data.frame.data.frame

#'@export
summary.spectral_spews_test <- function(x) { 
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
  cat(" Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n')
  cat('\n')

  invisible(x)
}


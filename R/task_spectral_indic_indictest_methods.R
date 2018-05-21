# 
# 
# File that contains methods for spectral_indictest objects
# 


# Note: we reuse the print method for spectral sews that works well
#'@method print spectral_sews_test
#'@export 
print.spectral_sews_test <- function(x, ...) { 
  print.spectral_sews(x, ...)
}

# Note: spectral_sews_test objects are already data.frames. We explicitely 
#   assign the method here in case we want to modify it later
#' @method as.data.frame spectral_sews_test
#' @export
as.data.frame.spectral_sews_test <- as.data.frame.data.frame

#'@export
summary.spectral_sews_test <- function(object, ...) { 
  cat('Spectral Spatial Early-Warnings results\n') 
  cat('\n')
  
  # Keep only SDR and print as data frame
  x2 <- as.data.frame(object)
  x2 <- x2[x2[ ,"type"] == 'sdr', c('replicate', 'value', 'pval')]
  
  # Format pvals
  x2<- data.frame(x2, stars = pval_stars(x2[ ,'pval']))
  x2[ ,"pval"] <- format_pvalues(x2[ ,"pval"], attr(object, "nreplicates"))
  
  names(x2) <- c('Matrix #', 'SDR Value', 'P>null', '   ')
  print.data.frame(x2, row.names = FALSE)
  
  cat('\n')
  cat(' Significance tested against', attr(object, 'nreplicates'), 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n')
  cat('\n')

  invisible(object)
}


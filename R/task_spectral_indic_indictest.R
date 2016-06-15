
# 
# Indictest functions for spectral_spews objects.
# 
#'@export
indictest.spectral_spews_list <- function(x, null_replicates = 999, ...) { 
  
  # Compute a distribution of null values for SDR
  results <- plyr::llply(x, indictest.spectral_spews_single, 
                             null_replicates, ...)
  
  # Add a replicate column with replicate number
  results <- Map(function(x, df) { df[ ,'replicate'] <- x; df }, 
                 seq.int(length(results)), results)
  
  # Bind all of it in a single df
  results <- do.call(rbind, results)
  
  # Format and return output
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}

#'@export
indictest.spectral_spews_single <- function(x, null_replicates = 999, ...) { 
  
  # Build closure passed to compute_indicator_with_null that uses the correct
  #   high and low ranges.
  sdr_indicf <- function(mat) { 
    indicator_sdr_core(mat, 
                       low_range  = x[['sdr_low_range']], 
                       high_range = x[['sdr_high_range']])
  }
  
  # Compute a distribution of null values for SDR
  null_values_sdr <- 
    compute_indicator_with_null(x[['orig_data']], 
                                # We do not make use of detrending for SDR
                                detrending = FALSE,
                                nreplicates = null_replicates, 
                                indicf = sdr_indicf)
  
  # Compute a distribution of null values for SDR
  # rspec_null returns a vector instead of a data.frame and is thus 
  #   compatible with compute_indicator_with_null that uses replicate 
  #   internally.
  rspec_null <- function(mat) rspectrum(mat)[ ,'rspec']
  
  null_values_spectrum <- 
    compute_indicator_with_null(x[['orig_data']], 
                                detrending = FALSE,
                                nreplicates = null_replicates, 
                                indicf = rspec_null)
  
  # Format and return result. We always add a replicate column that can be 
  #   read later on by the plot methods. 
  results <- 
    plyr::rbind.fill(data.frame(replicate = 1, 
                                type = 'sdr', 
                                null_values_sdr), 
                     data.frame(replicate = 1, 
                                type = 'rspectrum', 
                                dist = x[['results']][['spectrum']][ ,'dist'], 
                                null_values_spectrum))
  
  class(results) <- c('spectral_spews_test', 'spews_test', 'data.frame')
  
  return(results)
}

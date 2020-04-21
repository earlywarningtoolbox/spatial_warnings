# 
# 
# This file contains the indictest functions for spectral sews
# 

#' @rdname spectral_sews
#' 
#' @param nulln Number of simulations used to produce the null 
#'   distribution of indicator values.
#' 
#' @export
indictest.spectral_sews <- function(x, 
                      nulln = 999, 
                      null_method = 'perm', 
                      ...) { 
  NextMethod('indictest')
}

# 
# Indictest functions for spectral_sews objects.
#' @method indictest spectral_sews_list
#'@export
indictest.spectral_sews_list <- function(x, 
                                         nulln = 999, 
                                         null_method = 'perm', 
                                         ...) { 
  
  # Compute a distribution of null values for SDR
  results <- future.apply::future_lapply(x, indictest.spectral_sews_single, 
                                         nulln, null_method, ...)
  
  # Format and return output
  class(results) <- c('spectral_sews_test_list', 
                      'spectral_sews_list', 
                      'simple_sews_test_list', 
                      'simple_sews_list', 
                      'sews_result_list')
  
  return(results)
}

#' @method indictest spectral_sews_single
#' @export
indictest.spectral_sews_single <- function(x, 
                                           nulln = 999, 
                                           null_method = 'perm', 
                                           ...) { 
  
  # Build closure passed to compute_indicator_with_null that uses the correct
  #   high and low ranges, and is compatible with the use of matrixn(). 
  sdr_indicf <- function(mat) { 
    spectrum <- rspectrum(mat)
    
    c(sdr = indicator_sdr_do_ratio(spectrum, x[['low_range']], 
                                   x[['high_range']]), 
      spectrum = spectrum[ ,'rspec'])
  }
  
  # Compute a distribution of null values for SDR
  test_values <- 
    compute_indicator_with_null(x[['orig_data']], 
                                nulln = nulln, 
                                indicf = sdr_indicf, 
                                null_method = null_method)
  
  # Format results. The first four values are parameters, the rest is the 
  # variogram.
  sdr_values <- llply(test_values, function(o) o[1])
  x[names(sdr_values)] <- sdr_values
  
  spec <- as.data.frame(do.call(cbind, 
                                lapply(test_values, function(o) o[-1])))
  spec <- data.frame(x[["spectrum"]], spec)
  row.names(spec) <- as.character(seq.int(nrow(spec)))
  
  # We replace or add things in x
  x[["spectrum"]] <- spec
  x[["nulln"]] <- nulln
  x[["null_method"]] <- null_method
  
  class(x) <- c('spectral_sews_test_single', 
                'spectral_sews_single', 
                'simple_sews_test_single', 
                'simple_sews_single', 
                'sews_result_single')
  
  return(x)
}

# Plot function for r-spectrum
# 
# We define the S3 method. Note that args are already defined in plot() method
# 


# Methods to extract the spectrum from spectal_sews objects
# ---------------------------------------------------------

extract_spectrum <- function(x, ...) { 
  UseMethod("extract_spectrum")
}

extract_spectrum.spectral_sews_list <- function(x, ...) { 
  values <- Map(function(n, o) data.frame(matrixn = n, o[["spectrum"]]), 
                seq_along(x), x)
  do.call(rbind, values)
}
extract_spectrum.spectral_sews_single <- function(x, ...) { 
  x[["spectrum"]]
}

#' @title Spectrum plot
#'
#' @rdname spectral_sews
#' 
#' @export
plot_spectrum <- function(x, 
                          along = NULL, 
                          log = TRUE, 
                          display_null = TRUE, 
                          ...) { 
  UseMethod("plot_spectrum")
}

# Method for indictest output
#'@export
plot_spectrum.spectral_sews_test_list <- function(x, 
                                                  along = NULL, 
                                                  log = TRUE, 
                                                  display_null = TRUE, 
                                                  ...) { 
  
  ggobj <- plot_spectrum.spectral_sews_list(x, along, log)
  
  # Add layers with null model information
  ggobj$layers <- c(geom_line(aes_string(x = "dist", y = "null_mean"), 
                              color = 'black', alpha = .1), 
                    geom_ribbon(aes_string(x = "dist", ymin = "null_05", 
                                           ymax = "null_95"), 
                                fill = 'grey',
                                group = 1, 
                                alpha = .8), 
                    ggobj$layers)
  
    
  return(ggobj) 
}

#'@export
plot_spectrum.spectral_sews_test_single <- function(x, 
                                                  along = NULL, 
                                                  log = TRUE, 
                                                  display_null = TRUE, 
                                                  ...) { 
  
  # Get base plot 
  ggobj <- plot_spectrum.spectral_sews_single(x, along, log)
  
  # Add layers with null model information
  ggobj$layers <- c(geom_line(aes_string(x = "dist", y = "null_mean"), 
                              color = 'black', alpha = .1), 
                    geom_ribbon(aes_string(x = "dist", ymin = "null_05", 
                                           ymax = "null_95"), 
                                fill = 'grey',
                                group = 1, 
                                alpha = .8), 
                    ggobj$layers)
  
  return(ggobj)
}

# Method for spectral_sews output (list object)
#'@export
plot_spectrum.spectral_sews_list <- function(x, 
                                             along = NULL, 
                                             log = TRUE, 
                                             display_null = TRUE, 
                                             ...) { 
  
  tab <- extract_spectrum(x)
  check_suitable_for_plots(tab, along)
  
  if ( ! is.null(along) ) { 
    tab[ ,"along"] <- along[tab[ ,"matrixn"]]
  } else { 
    tab[ ,"along"] <- tab[ ,"matrixn"]
  }
  
  p <- ggplot(tab, aes_string(x = "dist", y = "rspec")) + 
    geom_line() + 
    theme_spwarnings() + 
    facet_wrap( ~ along ) + 
    labs(x = "Distance", y = "r-spectrum")
  
  if (log) { 
    p <- p + scale_y_continuous(trans = "log10")
  }
  return(p)
}

# Method for spectral_sews output (single object)
#'@export
plot_spectrum.spectral_sews_single <- function(x, 
                                               along = NULL, 
                                               log = TRUE, 
                                               display_null = TRUE, 
                                               ...) { 
  p <- ggplot(extract_spectrum(x), 
         aes_string(x = "dist", y = "rspec")) + 
    geom_line() + 
    theme_spwarnings() + 
    labs(x = "Distance", y = "r-spectrum")
  
  if (log) { 
    p <- p + scale_y_continuous(trans = "log10")
  }
  
  return(p)
}

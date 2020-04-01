# 
#' @rdname spectral_sews
#' 
#' @param x A \code{spectral_sews_test} object as produced by \link{indictest}
#' 
#' @param along An optional vector of values along which the trend is to be 
#'   displayed. The length of the vector must be equal to the number of 
#'   indicator values provided in the object \code{x}.
#' 
#' @param what What the trend-line to be displayed should represent about the 
#'   indicator. Defaults to the indicator's values ("value") but other metrics 
#'   can be displayed. Correct values are "value", "pval" or "z_score".
#' 
#' @param display_null Sets whether a grey ribbon should be added to reflect
#'   the null distribution. Note that it can not be displayed when the trend 
#'   line reflects something else than the indicator values (when \code{what} 
#'   is not set to "value").
#' 
#' @param ... Ignored 
#' 
#' @method plot spectral_sews_test
#' @export
plot.spectral_sews_test <- function(x, # an indictest object
                                     ..., 
                                     along = NULL, 
                                     what = 'value', 
                                     display_null = TRUE) { 
  
  # If along is not provided, then use the matrixn number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(x[ ,"matrixn"])
    set_default_xlab <- TRUE 
  }
  
  check_suitable_for_plots(x, along, display_null)
  
  # This function only plots summary sdr so we subset the indictest data.frame
  is_sdr <- x[ ,'type'] == 'sdr'
  plot_data <- data.frame(x[is_sdr, ],
                          gradient = along[x[is_sdr,'matrixn']])
  
  # Create base plot object 
  plot <- ggplot(plot_data) + theme_spwarnings()
  
  # Check if we really want to add a null ribbon
  add_null <- display_null
  if ( display_null && ! "null_mean" %in% colnames(x) ) { 
    warning('Null data was specified to be displayed but could not be found ', 
            'in the provided object')
    add_null <- FALSE
  }
  
  if ( display_null && what != "value" ) { 
    warning('Cannot display null model quantiles when the indicator value is ',
            'not displayed')
    add_null <- FALSE
  }
  
  if ( add_null ) { 
    null_data <- data.frame(plot_data,
                            null_ymin = x[is_sdr,'null_05'],
                            null_ymax = x[is_sdr,'null_95'])
    
    plot <- plot + 
      geom_ribbon(aes_string(x = 'gradient',
                             ymin = 'null_ymin',
                             ymax = 'null_ymax'),
                  data = null_data, 
                  fill = 'grey',
                  alpha = .8) + 
      geom_line(aes_string(x = "gradient", 
                           y = "null_mean"), 
                color = 'black', alpha = .1)
  }
  
  # Add the trend on the graph (Note that we add it over the null trend)
  plot <- plot + 
    geom_point(aes_string(x = 'gradient', y = what)) + 
    geom_line(aes_string(x = 'gradient', y = what))
  
  # Add ylabs
  plot <- plot + ylab('Spectral density ratio')
  
  # Add names
  if ( set_default_xlab ) { 
    plot <- plot + xlab('Matrix number')
  } else { 
    plot <- plot + xlab(as.character(match.call()['along']))
  }
  
  return(plot)
}

#' @export
#' @method plot spectral_sews_list
plot.spectral_sews_list <- function(x, ..., along = NULL) { 
  plot.spectral_sews_test(as.data.frame(x), 
                           along = along,
                           display_null = FALSE, 
                           what = 'value')
}





# Plot function for r-spectrum
# 
# We define the S3 method. Note that args are already defined in plot() method
# 
#' @title Spectrum plot
#'
#' @rdname spectral_sews
#' 
#' @export
plot_spectrum <- function(x, along = NULL, display_null = TRUE) { 
  UseMethod("plot_spectrum")
}

# Method for indictest output
#' @export
plot_spectrum.spectral_sews_test <- function(x, 
                                             along = NULL, 
                                             display_null = TRUE) { 
  
  # If along is not provided, then use the replicate number
  if ( !is.null(along) && (length(along) != max(x[ ,'matrixn'])) ) { 
    stop('The along values are unfit for plotting (size mismatch)')
  }
  
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(x[ ,"matrixn"])
  }
  
  # We subset the original object to use only rspectrum-related variables and 
  # add the gradient variable to it. We also add a matrixn column 
  is_rspec <- x[ ,'type'] == 'rspectrum'
  x <- data.frame(x[is_rspec, ], 
                    gradient = along[x[is_rspec, 'matrixn']])
  
  # Create base plot. 
  plot <- ggplot(x) +
            ylab('r-spectrum value') + 
            xlab('Distance (cell size unit)') + 
            theme_spwarnings()
    
  # If we are in correct conditions to add a null values ribbon
  if ( display_null ) { 
    plot <- plot + 
      geom_ribbon(aes_q(x = ~dist,
                        ymin = ~null_05,
                        ymax = ~null_95),
                  fill = 'grey',
                  alpha = .8) + 
      geom_line(aes_q(x = ~dist, 
                                        y = ~null_mean), 
                         color = 'black', alpha = .1)
  }
  
  # Add layer for the observed spectrum
  plot <- plot + geom_line(aes_q(x = ~dist, 
                                                   y = ~value, 
                                                   group = ~matrixn)) +
    scale_color_gradient(low = '#000000', high = '#E86435', 
                                  name = 'Spectral \nDensity \nRatio') 
   
  # Add facets if multiple matrices are present
  if ( length(unique(x[ ,"matrixn"])) > 1 ) {
    plot <- plot + facet_wrap( ~ gradient ) 
  }
  
  return(plot) 
}

# Method for spectral_sews output (list object)
#' @export
plot_spectrum.spectral_sews_list <- function(x, 
                                              along = NULL, 
                                              ...) { 
  data_as_df <- as.data.frame(x)
  plot_spectrum.spectral_sews_test(data_as_df, along = along, 
                                    display_null = FALSE)
}

# Method for spectral_sews output (single object)
#' @export
plot_spectrum.spectral_sews_single <- function(x, ...) { 
  data_as_df <- as.data.frame(x)
  plot_spectrum.spectral_sews_test(data_as_df, display_null = FALSE)
}

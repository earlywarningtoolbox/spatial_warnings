# 
#' @title Spectral spatial early-warning signals: SDR plotting function
#' 
#' @rdname spectral_spews
#' 
#' @param obj An \code{spectral_spews_test} object as produced by \link{indictest}
#' 
#' @param along An optional vector of values along which the trend is to be 
#'   displayed. The length of the vector must be equal to the number of 
#'   indicator values provided in the object \code{obj}.
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
plot.spectral_spews_test <- function(obj, # an indictest object
                                     along = NULL, 
                                     what = 'value', 
                                     display_null = TRUE) { 
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(obj[ ,"replicate"])
    set_default_xlab <- TRUE 
  }
  
  check_suitable_for_plots(obj, along, display_null)
  
  # This function only plots summary sdr so we subset the indictest data.frame
  is_sdr <- obj[ ,'type'] == 'sdr'
  plot_data <- data.frame(obj[is_sdr, ],
                          gradient = along[obj[is_sdr,'replicate']])
  
  # Create base plot object 
  plot <- ggplot2::ggplot(plot_data)
  
  # Check if we really want to add a null ribbon
  add_null <- display_null
  if ( display_null && ! "null_mean" %in% colnames(obj) ) { 
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
                            null_ymin = obj[is_sdr,'null_05'],
                            null_ymax = obj[is_sdr,'null_95'])
    
    plot <- plot + 
      ggplot2::geom_ribbon(ggplot2::aes_string(x = 'gradient',
                                               ymin = 'null_ymin',
                                               ymax = 'null_ymax'),
                           data = null_data, 
                           fill = 'grey',
                           alpha = .8) + 
      ggplot2::geom_line(ggplot2::aes_string(x = "gradient", 
                                             y = "null_mean"), 
                         color = 'black', alpha = .1)
      
      
  }
  
  # Add the trend on the graph (Note that we add it over the null trend)
  plot <- plot + ggplot2::geom_line(ggplot2::aes_string(x = 'gradient', 
                                                        y = what))
  
  # Add ylabs
  plot <- plot + ggplot2::ylab('Spectral density ratio')
  
  # Add names
  if ( set_default_xlab ) { 
    plot <- plot + ggplot2::xlab('Matrix number')
  } else { 
    plot <- plot + ggplot2::xlab(as.character(match.call()['along']))
  }
  
  return(plot)
}

plot.spectral_spews_list <- function(obj, along = NULL) { 
  plot.spectral_spews_test(as.data.frame(obj), 
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
#' @rdname spectral_spews
#' 
plot_spectrum <- function(obj, along = NULL, display_null = TRUE) { 
  UseMethod("plot_spectrum")
}

# Method for indictest output
#'@export
plot_spectrum.spectral_spews_test <- function(obj, 
                                              along = NULL, 
                                              display_null = TRUE) { 
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(obj[ ,"replicate"])
  }
  
  # We subset the original object to use only rspectrum-related variables and 
  # add the gradient variable to it. We also add a replicate column 
  is_rspec <- obj[ ,'type'] == 'rspectrum'
  obj <- data.frame(obj[is_rspec, ], 
                    gradient = along[obj[is_rspec, 'replicate']])
  
  # Create base plot. 
  plot <- ggplot2::ggplot(obj) +
            ggplot2::ylab('r-spectrum value') + 
            ggplot2::xlab('Distance (cell size unit)')
    
  # If we are in correct conditions to add a null values ribbon
  if ( display_null ) { 
    plot <- plot + 
      ggplot2::geom_ribbon(ggplot2::aes_q(x = ~dist,
                                          ymin = ~null_05,
                                          ymax = ~null_95),
                           fill = 'grey',
                           alpha = .8) + 
      ggplot2::geom_line(ggplot2::aes_q(x = ~dist, 
                                        y = ~null_mean), 
                         color = 'black', alpha = .1)
  }
  
  # Add layer for the observed spectrum
  plot <- plot + ggplot2::geom_line(ggplot2::aes_q(x = ~dist, 
                                                   y = ~value, 
                                                   group = ~replicate)) +
    ggplot2::scale_color_gradient(low = '#000000', high = '#E86435', 
                                  name = 'Spectral \nDensity \nRatio') 
   
  # Add facets if multiple replicates are present
  if ( length(unique(obj[ ,"replicate"])) > 1 ) {
    plot <- plot + ggplot2::facet_wrap( ~ gradient ) 
  }
  
  return(plot) 
}

# Method for spectral_spews output (list object)
plot_spectrum.spectral_spews_list <- function(obj, 
                                              along = NULL) { 
  data_as_df <- as.data.frame(obj)
  plot_spectrum.spectral_spews_test(data_as_df, along = along, 
                                    display_null = FALSE)
}

# Method for spectral_spews output (single object)
plot_spectrum.spectral_spews_single <- function(obj) { 
  data_as_df <- as.data.frame(obj)
  plot_spectrum.spectral_spews_test(data_as_df, display_null = FALSE)
}

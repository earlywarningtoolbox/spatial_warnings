
# Plot function for SDR
#'@export
plot.spectral_spews_test <- function(obj, 
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
  
  plot_data <- data.frame(obj, gradient = along[obj[ ,'replicate']])
  
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
                            null_ymin = obj[ ,'null_05'],
                            null_ymax = obj[ ,'null_95'])
    
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
  plot <- plot + ggplot2::ylab('SDR value')
  
  # Add names
  if ( set_default_xlab ) { 
    plot <- plot + ggplot2::xlab('Matrix number')
  } else { 
    plot <- plot + ggplot2::xlab(as.character(match.call()['along']))
  }
  
  return(plot)
}

# Plot function for r-spectrum
# 
#'@export
plot_spectrum <- function(obj, ...) { 
  UseMethod("plot_spectrum")
}

#'@export
plot_spectrum.spectral_spews_list <- function(obj, 
                          along = NULL, 
                          add.facets = TRUE) { 
  
  if ( ! inherits(obj, 'spectral_spews_list') ) { 
    
  }
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- seq.int(length(obj))
    set_default_xlab <- TRUE 
  }
  
  if ( length(obj) != length(along) ) { 
    stop('External data length (along = ...) does not match ',
         'the number of replicates !')
  }
  
  # Massage data so it fits the ggplot format
  all_spectra <- plyr::adply(seq.int(obj), 1, function(i) { 
      data.frame(replicate = i, 
                 along = along[i],
                 obj[[i]][['results']][['spectrum']], 
                 sdr = obj[[i]][['results']][['sdr']])
    })
  
  # Create base plot
  plot <- ggplot2::ggplot(all_spectra) + 
            ggplot2::ylab('R-spectrum value')
  
  # Add layer
  if ( add.facets ) { 
    plot <- plot + ggplot2::geom_line(ggplot2::aes_q(x = ~dist, 
                                                     y = ~rspec, 
                                                     color = ~sdr,
                                                     group = ~replicate)) + 
              ggplot2::facet_wrap( ~ along ) + 
              ggplot2::scale_color_gradient(low = '#000000', high = '#E86435', 
                                            name = 'Spectral \nDensity \nRatio') 
  } else { 
    plot <- plot + 
      ggplot2::geom_line(ggplot2::aes_q(x = ~dist, 
                                        y = ~rspec, 
                                        color = ~along, 
                                        group = ~replicate))
  }
  
  # Add names
  if ( set_default_xlab ) { 
    plot <- plot + ggplot2::xlab('Matrix number')
  } else { 
    plot <- plot + ggplot2::xlab(as.character(match.call()['along']))
  }
  
  return(plot) 
}

#@export
plot_spectrum.spectral_spews_single <- function(obj) { 
  
  dat <- data.frame(obj[['results']][['spectrum']], 
                    sdr = obj[['results']][['sdr']])
  
  ggplot2::ggplot(dat) + 
    ggplot2::geom_line(ggplot2::aes_q(x = ~dist, 
                                      y = ~rspec))
  
}

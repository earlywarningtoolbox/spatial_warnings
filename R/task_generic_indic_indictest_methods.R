# 
# 
# This file contains common methods (plot/print/summary/as.data.frame) used for 
#   generic indicators (object with significance assessment 'indictest')
# 




# Plot method
# --------------------------------------------------

#'@export
plot.generic_spews_test <- function(obj, 
                                    along = NULL, 
                                    what = 'value',
                                    display_null = TRUE, 
                                    ...) {  
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(obj[ ,"replicate"])
    set_default_xlab <- TRUE 
  }
  
  check_suitable_for_plots(obj, along, display_null)
  
  plot_data <- data.frame(as.data.frame(obj), 
                          gradient = along[obj[ ,'replicate']])
  
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
                                                        y = what,
                                                        color = 'indicator', 
                                                        group = 'indicator'))
  
  # Add facets 
  plot <- plot + 
            ggplot2::facet_wrap( ~ indicator, scales = 'free_y') + 
            ggplot2::guides(color = FALSE) # Disable color legend
  
  # Add names
  if ( set_default_xlab ) { 
    plot <- plot + ggplot2::xlab('Matrix number')
  } else { 
    plot <- plot + ggplot2::xlab(as.character(match.call()['along']))
  }
  
  return(plot)
}



# as.data.frame methods
# --------------------------------------------------
as.data.frame.generic_spews_test <- function(obj) { 
  
  # The list methods actually works pretty well so we do this. However, 
  # we define the method instead of relying on automatic dispatch 
  # in case we want to change it later
  as.data.frame.list(obj) 
}



# Print method
# --------------------------------------------------
 
#'@export
print.generic_spews_test <- function(x) { 
  cat('Generic Spatial Early-Warnings\n') 
  cat('\n')
  
  # Reformat table
  x2 <- as.data.frame(x)
  x2 <- data.frame(x2, stars = pval_stars(x2[ ,'pval']))
  x2 <- dlply(x2, ~ indicator, subset, select = c('value', 'pval', 'stars'))
  
  # We just keep the value for the mean (pval makes no sense)
  x2[['Mean']] <- x2[['Mean']][ ,c('value')]
  
  # Format final table
  x2 <- data.frame(replicate = unique(x[ ,'replicate']), 
                   do.call(data.frame, x2))
                     
  names(x2) <- c('Replicate', 'Mean', 
                 'Moran\'s I', 'P>null', '   ',
                 'Skewness', 'P>null', '   ',
                 'Variance', 'P>null', '   ')
  
  print.data.frame(x2, row.names = FALSE)
  
  cat('\n')
  cat(' Significance tested against', attr(x, 'nreplicates'), 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", '\n')
  cat('\n')

  invisible(x)
}



# Summary method
# --------------------------------------------------
summary.generic_spews_test <- print.generic_spews_test


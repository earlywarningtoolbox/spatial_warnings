# 
# 
# This file contains common methods (plot/print/summary/as.data.frame) used for 
#   generic indicators (object with significance assessment 'indictest')
# 




# Plot methods
# --------------------------------------------------
# 
# 
#' @title Generic spatial early-warning signals: plotting function
#' 
#' @rdname generic_spews
#'
#' @param obj A \code{generic_spews} object (as provided by the 
#'   \code{generic_spews} function). 
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. If \code{NULL} then the values are plotted sequentially 
#'   in their original order. 
#' 
#' @param what The trendline to be displayed. Defaults to the indicator's 
#'   values ("value") but other metrics can be displayed. Correct values are 
#'   "value", "pval" or "z_score".
#' 
#' @param display_null Chooses whether a grey ribbon should be added to reflect
#'   the null distribution. Note that it can not be displayed when the trend 
#'   line reflects something else than the indicator values (when \code{what} 
#'   is not set to "value").
#' 
#' @method plot generic_spews_test
#' @export
plot.generic_spews_test <- function(x, 
                                    along = NULL, 
                                    what = 'value',
                                    display_null = TRUE, 
                                    ...) {  
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(x[ ,"replicate"])
    set_default_xlab <- TRUE 
  }
  
  check_suitable_for_plots(x, along, display_null)
  
  plot_data <- data.frame(as.data.frame(x), 
                          gradient = along[x[ ,'replicate']])
  
  # Change indicator names to prettier names before plotting 
  levels(plot_data[ ,'indicator']) <- c('Mean', 'Moran\'s I', 
                                        'Skewness', 'Variance')
  
  # Create base plot object 
  plot <- ggplot2::ggplot(plot_data) + 
    linescale_spwarnings() + 
    theme_spwarnings() 
  
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
                            null_ymin = x[ ,'null_05'],
                            null_ymax = x[ ,'null_95'])
    
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
  plot <- plot + 
    ggplot2::geom_point(ggplot2::aes_string(x = 'gradient', 
                                            y = what,
                                            color = 'indicator')) + 
    ggplot2::geom_line(ggplot2::aes_string(x = 'gradient', 
                                           y = what,
                                           color = 'indicator', 
                                           group = 'indicator'))
  
  # Add facets 
  plot <- plot + 
            facet_wrap( ~ indicator, scales = 'free_y') + 
            guides(color = FALSE) + # Disable color legend (redundant with facets)
            ylab('Indicator value') 
  
  # Add x-axis names 
  if ( set_default_xlab ) { 
    plot <- plot + 
      ggplot2::xlab('Matrix number') 
  } else { 
    plot <- plot + ggplot2::xlab(as.character(match.call()['along']))
  }
  
  return(plot)
}



# as.data.frame methods
# --------------------------------------------------
#'@export
as.data.frame.generic_spews_test <- function(x, ...) { 
  
  # The list methods actually works pretty well so we do this. However, 
  # we define the method instead of relying on automatic dispatch 
  # in case we want to change it later
  as.data.frame.list(x) 
}



# Print method
# --------------------------------------------------
 
#'@export
print.generic_spews_test <- function(x, ...) { 
  cat('Generic Spatial Early-Warnings\n') 
  cat('\n')
  
  # Reformat table
  x2 <- as.data.frame(x)
  x2 <- data.frame(x2, stars = pval_stars(x2[ ,'pval']))
  x2 <- dlply(x2, ~ indicator, subset, select = c('value', 'pval', 'stars'))
  
  # We just keep the value for the mean (pval makes no sense)
  x2[['mean']] <- x2[['mean']][ ,c('value')]
  
  # Format final table
  x2 <- data.frame(replicate = unique(x[ ,'replicate']), 
                   do.call(data.frame, x2))
                     
  names(x2) <- c('Mat. #', 'Mean', 
                 'Moran\'s I', 'P>null', '   ',
                 'Skewness', 'P>null', '   ',
                 'Variance', 'P>null', '   ')
  
  print.data.frame(x2, row.names = FALSE, digits = DIGITS)
  
  cat('\n')
  cat(' Significance tested against', attr(x, 'nreplicates'), 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n')
  cat('\n')

  invisible(x)
}



# Summary method
# --------------------------------------------------
#'@export
summary.generic_spews_test <- function(object, ...) { 
  print.generic_spews_test(object, ...)
}


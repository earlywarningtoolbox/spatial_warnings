# 
# 
# This file contains common methods (plot/print/summary/as.data.frame) used for 
#   generic indicators. 
# 

# ----------------------------
# PLOT METHODS
# ----------------------------
# 
#' @title Plot the results of a generic spatial warning \code{indictest} result
#' 
#' @description Plotting functions for generic early warning signals
#' 
#' @param obj A \code{generic_spews_test} object (as provided by the 
#'   \code{\link{indictest}} function.
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. 
#' 
#' @param what The variable to plot. The default is to plot the value of the 
#'   indicator, but other variables can be chosen (e.g. p-value). 
#' 
#' @param display_null Controls the display of a grey ribbon representing the 
#'   95% and 5% quantile of the null distribution
#'   
#' @return A ggplot object (usually displayed immediatelly when called at the 
#'   prompt). 
#' 
#' @details Since this function returns a ggplot object, it can be later 
#'   modified to add other graphical elements (e.g. axis names or annotations). 
#' 
#' @seealso \code{\link{generic_spews}}, \code{\link{indictest.generic_spews}}
#'
#' @examples 
#' data(forestdat)
#' result <- generic_spews( as.binary_matrix(forestdat[['matrices']]) )
#' plot(indictest(result))
#'
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

# 
# 
# This function prints a pretty table of a generic_spews_test object
# 
#'@export
print.generic_spews_test <- function(x, ...) { 
  cat('Generic Spatial Early-Warnings Summary\n') 
  cat('\n')

  x2 <- as.data.frame(x)
  x2 <- subset(x2, select=c("replicate", "indicator", "value", "pval"))
  x2<- data.frame(x2, stars = pval_stars(x2[ ,'pval']))
  x2 <- melt(x2, id.vars=c("replicate", "indicator"))
  x2 <- dcast(x2, replicate ~ indicator + variable)

  names(x2) <- c(
	  'Replicate #', 'Mean', 'P>null', '   ',
	  'Moran\'s I', 'P>null', '   ',
	  'Skewness', 'P>null', '   ',
	  'Variance', 'P>null', '   '
	  )

  print.data.frame(x2, row.names = FALSE)
  
  cat('\n')
  cat(' Significance tested against', attr(x, 'nreplicates'), 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1", '\n')
  cat('\n')

  invisible(x)

}

print_one_replicate <- function(tab, n) { 

  f <- function(v) { sprintf(paste0('%3.4f'), v) }
  
  n_str <- paste0('    ',
                  sprintf("%3.0f", n))        # 1 char
  
  dat <- subset(tab, indicator == 'Mean')
  mean_str <- paste0('  ',
                     f(dat[ ,'value']),          # 7 chars
                     '')
  
  dat <- subset(tab, indicator == 'Variance')
  variance_str <- paste0('     ',                       # 1 char
                         f(dat[ ,'value']),          # 7 chars
                         '   ',                        # 1 char
                         f(dat[ ,'pval']),
                         ' ',
                         pval_stars(dat[ ,'pval'])) # 4 chars
  
  dat <- subset(tab, indicator == 'Skewness')
  skewness_str <- paste0('  ',
                         f(dat[ ,'value']),          # 7 chars
                         '   ',                        # 1 char
                         f(dat[ ,'pval']),
                         ' ',
                         pval_stars(dat[ ,'pval'])) # 4 chars
  dat <- subset(tab, indicator == 'Moran\'s I')
  moran_str <- paste0('  ',
                      f(dat[ ,'value']),          # 7 chars
                      '   ',                        # 1 char
                      f(dat[ ,'pval']),
                      ' ',
                      pval_stars(dat[ ,'pval'])) # 4 chars
  
  cat(paste0(n_str, mean_str, variance_str, skewness_str, moran_str, "\n"))
  
}

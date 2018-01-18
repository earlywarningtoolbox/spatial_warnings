# 
# This file contains a function to test significance of custom indicators 
# 
# WARNING: indictest will always transform original matrix to numeric 
# internally, be careful when your function works with logical data. 
# 
#' @rdname create_indicator
#' 
#' @param nperm The number of replicates to use to compute use in the 
#'   null distribution
#' 
#' @export
indictest.custom_spews <- function(x, nperm = 999, ...) { 
  NextMethod('indictest')
}

indictest.custom_spews_single <- function(x, nperm = 999, ...) { 
  
  # We do not support low numbers of replicates
  if ( nperm < 3 ) { 
    stop('The number of null replicates should be above 3 to ', 
         'assess significance')
  }
  
  new_indicf <- function(mat) { 
    do.call(x[['indicf']], c(mat = list(mat), x[['fun.args']]))
  }
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(x[["orig_data"]],
                                             nreplicates = nperm, 
                                             indicf = new_indicf)
  
  # Format result
  results <- c(null_values, x["fun.name"], list(nperm = nperm))
  class(results) <- c('custom_spews_test_single', 'spews_test', 'list')
  
  return(results)
}

indictest.custom_spews_list <- function(x, nperm = 999, ...) { 
  
  results <- parallel::mclapply(x, indictest.custom_spews_single, 
                                nperm, ...)
  
  # Add replicate column with correct replicate number
  for ( nb in seq_along(results) ) { 
    results[[nb]][['replicate']] <- nb
  }
  
  class(results) <- c('custom_spews_test_list', 'spews_test', 'list')
  return(results)
}



as.data.frame.custom_spews_test_single <- function(x, ...) { 
  # We need to explicitely add a `replicate` column because it will 
  # be used by funs down the stream. 
  data.frame(replicate = 1, as.data.frame.list(x))
}
as.data.frame.custom_spews_test_list <- function(x, ...) { 
  tab <- ldply(x, as.data.frame.list)
  # Reorder cols
  tab <- data.frame(replicate = tab[ ,'replicate'], 
                    tab[ ! names(tab) %in% 'replicate'])
  tab
}




summary.custom_spews_test_single <- function(object, ...) { 
  summary.custom_spews_test_list( list(object) )
}
summary.custom_spews_test_list <- function(object, ...) { 
  
  tab <- as.data.frame(object)
  
  # Get some info about the computation
  fun.name <- as.character(tab[1, "fun.name"])
  nperm    <- tab[1, "nperm"]
  
  # Format pvalues 
  tab[ ,'stars'] <- pval_stars(tab[ ,'pval'])
  
  tab <- tab[ ,c('replicate', 'value', 'pval', 'stars')]
  names(tab) <- c('Mat. #', fun.name, 'P>null', "")
  
  cat('Custom Spatial Early-Warnings:', fun.name, '\n') 
  cat('\n')
  print.data.frame(tab, row.names = FALSE, digits = DIGITS)
  cat('\n')
  cat(' Significance tested against', nperm, 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n')
  cat('\n')
  
  invisible(tab)
}




print.custom_spews_test_single <- function(x, ...) { 
  summary.custom_spews_test_single(x, ...)
}
print.custom_spews_test_list <- function(x, ...) { 
  summary.custom_spews_test_list(x, ...)
}



#' @rdname create_indicator
# /!\ along is already documented elsewhere !
# /!\ x is already documented elsewhere !
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
#' @method plot custom_spews_test
#' @export
plot.custom_spews_test <- function(x, 
                                   along = NULL,
                                   what = "value", 
                                   display_null = TRUE, 
                                   ...) { 
  NextMethod('plot')
}

#'@export
plot.custom_spews_test_single <- function(x, 
                                          along = NULL, 
                                          what = "value", 
                                          display_null = TRUE, 
                                          ...) { 
  stop('I cannot plot a trend with only one value')
}

#'@export
plot.custom_spews_test_list <- function(x, 
                                        along = NULL, 
                                        what = "value", 
                                        display_null = TRUE, 
                                        ...) { 
  
  # Transform into data.frame
  x <- as.data.frame(x)
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(x[ ,"replicate"])
    set_default_xlab <- TRUE 
  }
  
  check_suitable_for_plots(x, along, display_null)
  
  plot_data <- data.frame(as.data.frame(x), 
                          gradient = along[x[ ,'replicate']])
  
  # Create base ggplot2 object
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
                           group = 1, 
                           alpha = .8) + 
      ggplot2::geom_line(ggplot2::aes_string(x = "gradient", 
                                             y = "null_mean"), 
                         group = 1, 
                         color = 'black', alpha = .1)
  }
  
  
  # Add the trend on the graph (Note that we add it over the null ribbon)
  plot <- plot + 
    ggplot2::geom_point(ggplot2::aes_string(x = 'gradient', 
                                            y = what)) + 
    ggplot2::geom_line(ggplot2::aes_string(x = 'gradient', 
                                           y = what))
  
  # Set ylab 
  fun.name <- x[1, 'fun.name']
  plot <- plot + ylab(fun.name)
  
  # Set xlab
  if ( set_default_xlab ) { 
    plot <- plot + 
      ggplot2::xlab('Matrix number') 
  } else { 
    plot <- plot + ggplot2::xlab(as.character(substitute(along)))
  }
  
  return(plot)
}

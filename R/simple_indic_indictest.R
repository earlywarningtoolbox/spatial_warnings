# 
# This file contains functions to test significance and plot simple indicators 
# 
#' @export
indictest.simple_sews <- function(x, 
                                  nperm = 999, 
                                  null_method = c('perm', 'glm'), 
                                  covariate_layers = NULL, 
                                  ...) { 
  NextMethod('indictest')
}
#'@export
indictest.simple_sews_single <- function(x, 
                                         nperm = 999, 
                                         null_method = c('perm', 'glm'), 
                                         covariate_layers = NULL, 
                                         ...) { 
  
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
                                             indicf = new_indicf, 
                                             null_method = null_method, 
                                             covariate_layers = covariate_layers)
  
  # Format result
  results <- c(null_values, list(nperm = nperm))
  class(results) <- c('simple_sews_test_single', 'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  
  return(results)
}
#'@export
indictest.simple_sews_list <- function(x, 
                                       nperm = 999, 
                                       null_method = c('perm', 'glm'), 
                                       covariate_layers = NULL, 
                                       ...) { 
                                         
  results <- parallel::mclapply(x, indictest.simple_sews_single, 
                                nperm, null_method, covariate_layers, ...)
  
  # Add replicate column with correct replicate number
  for ( nb in seq_along(results) ) { 
    results[[nb]][['replicate']] <- nb
  }
  
  class(results) <- c('simple_sews_test_list', 'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  
  return(results)
}

#'@method as.data.frame simple_sews_test_single
#'@export
as.data.frame.simple_sews_test_single <- function(x, ...) { 
  # Find or create the indicator names
  indicnames <- ifNULLthen(names(x[['value']]), 
                           paste0("indic_", seq_along(x[[1]][['value']])))
  
  # We need to explicitely add a `replicate` column because it will 
  # be used by funs down the stream. 
  ans <- data.frame(replicate = 1, 
                    indic = indicnames, 
                    as.data.frame.list(x))
  
  return(ans)
}
#'@method as.data.frame simple_sews_test_list
#'@export
as.data.frame.simple_sews_test_list <- function(x, ...) { 
  # Find or create the indicator names
  indicnames <- ifNULLthen(names(x[[1]][['value']]), 
                           paste0("indic_", seq_along(x[[1]][['value']])))

  tab <- ldply(x, function(x) { data.frame(indic = indicnames, 
                                           as.data.frame.list(x)) } )
  # Reorder cols so that 'replicate' is first'
  tab <- data.frame(replicate = tab[ ,'replicate'], 
                    tab[ ! names(tab) %in% 'replicate'])
  tab
}


#'@method as.data.frame simple_sews_test_list
#'@export
summary.simple_sews_test_single <- function(object, ...) { 
  object.list <- list(object)
  attr(object.list, "indicname") <- attr(object, "indicname")
  summary.simple_sews_test_list( object.list )
}
#'@export
summary.simple_sews_test_list <- function(object, 
                                          indicname = attr(object, "indicname"), 
                                          ...) { 
  if ( is.null(indicname) ) { 
    indicname <- "unknown indicator(s)"
  }
  
  tab <- as.data.frame(object)
  
  # Get some info about the computation
  nperm <- tab[1, "nperm"]
  
  # Format pvalues 
  tab[ ,'stars'] <- pval_stars(tab[ ,'pval'])
  
  tab <- tab[ ,c('replicate', 'indic', 'value', 'pval', 'stars')]
  
  # Reshape the table 
  tab_pretty <- llply(unique(tab[['indic']]), function(current_indic) { 
    a <- subset(tab, indic == current_indic)[ ,c('value', 'pval', 'stars')]
    names(a) <- c(as.character(current_indic), 
                  paste0('pval_', current_indic), 
                  paste0('stars_', current_indic))
    a
  })
  tab_pretty <- do.call(data.frame, tab_pretty)
  tab_pretty <- data.frame(replicate = seq_along(nrow(tab_pretty)), tab_pretty)
  
  # Build the header to print. Note that we set it to the names of the 
  # data.frame. This is handy because then print.data.frame handles all the 
  # spacing for us, but we effectively create a data.frame with duplicated 
  # column names. I don't know how brittle this is, but probably very. 
  header <- names(tab_pretty) 
  header[grepl('pval_', header)] <- 'P>null'
  header[grepl('stars_', header)] <- ''
  header[grepl('replicate', header)] <- 'Mat #'
  names(tab_pretty) <- header
  
  cat('Spatial Early-Warning:', indicname, '\n') 
  cat('\n')
  print.data.frame(tab_pretty, row.names = FALSE, digits = DIGITS)
  cat('\n')
  cat(' Significance tested against', nperm, 
      'randomly shuffled matrices\n')
  cat(" Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n')
  cat('\n')
  
  invisible(tab)
}



#'@export
print.simple_sews_test_single <- function(x, ...) { 
  summary.simple_sews_test_single(x, ...)
}
#'@export
print.simple_sews_test_list <- function(x, ...) { 
  summary.simple_sews_test_list(x, ...)
}


#' @title Spatial early-warning signals: display of trends
#' 
#'
#' 
# /!\ along is already documented elswhere !
# /!\ x is already documented elswhere !
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
#' @method plot simple_sews_test
#' @export
plot.simple_sews_test <- function(x, 
                                  along = NULL,
                                  what = "value", 
                                  display_null = TRUE, 
                                  ...) { 
  NextMethod('plot')
}

#' @method plot simple_sews_test_single
#'@export
plot.simple_sews_test_single <- function(x, 
                                          along = NULL, 
                                          what = "value", 
                                          display_null = TRUE, 
                                          ...) { 
  stop('I cannot plot a trend with only one value')
}

#'@method plot simple_sews_test_list
#'@export
plot.simple_sews_test_list <- function(x, 
                                       along = NULL, 
                                       what = "value", 
                                       display_null = TRUE, 
                                       ...) { 
  
  # Transform into data.frame
  x.df <- as.data.frame(x)
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(x.df[ ,"replicate"])
    set_default_xlab <- TRUE 
  }
  
  check_suitable_for_plots(x.df, along, display_null)
  
  plot_data <- data.frame(as.data.frame(x.df), 
                          gradient = along[x.df[ ,'replicate']])
  
  # Create base ggplot2 object
  # Create base plot object 
  plot <- ggplot2::ggplot(plot_data) + 
            linescale_spwarnings() + 
            theme_spwarnings() + 
            facet_wrap(~ indic, scales = "free_y")
  
  # Check if we really want to add a null ribbon
  add_null <- display_null
  if ( display_null && ! "null_mean" %in% colnames(x.df) ) { 
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
                            null_ymin = x.df[ ,'null_05'],
                            null_ymax = x.df[ ,'null_95'])
    
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
  ylabel <- "Value"
  plot <- plot + ylab(ylabel)
  
  # Set xlab
  if ( set_default_xlab ) { 
    plot <- plot + 
      ggplot2::xlab('Matrix number') 
  } else { 
    plot <- plot + ggplot2::xlab(as.character(substitute(along)))
  }
  
  return(plot)
}

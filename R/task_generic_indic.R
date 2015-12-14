# 
# 
# Functions providing the global workflow for generic early-warning-signals
# 
# 





# ----------------------------
# COMPUTATION 
# ----------------------------

#' @title Generic spatial warning signals
#' 
#' @description Computation of spatial generic early warning signals (Moran's I, variance and skewness)
#' 
#' 
#' @references 
#'   Dakos, V., van Nes, E. H., Donangelo, R., Fort, H., & 
#'   Scheffer, M. (2010). Spatial correlation as leading indicator of 
#'   catastrophic shifts. Theoretical Ecology, 3(3), 163-174.
#'   
#'   Guttal, V., & Jayaprakash, C. (2008). Spatial variance and spatial 
#'   skewness: leading indicators of regime shifts in spatial ecological 
#'   systems. Theoretical Ecology, 2(1), 3–12. 
#'
#' @param mat A matrix (quantitative data), a binary matrix (qualitative data), 
#'   or a list of those
#' 
#' @param subsize The subsize for the coarse-graining in case the passed matrix
#'   is qualitative
#'   
#' @param detrend Whether to substract the mean or not to the input matrix 
#'   (detrend)
#' 
#' @return An object of class \code{generic_spews_single} if the passed object
#'   was a single matrix or an object of class \code{generic_spews_list} if 
#'   it was a list.
#' 
#' @details Before a critical point, a spatial dynamical system is expected to 
#'   show an increase in spatial correlation at lag-1 (measured by Moran's I), 
#'   in variance and in skewness. These functions provides a workflow to 
#'   compute and test those indicators (see 
#'   \code{\link{indicator_moran}}, \code{\link{indicator_variance}} and 
#'   \code{\link{indicator_skewness}} details about null models). 
#' 
#' @seealso \code{\link{indicator_moran}}, \code{\link{indicator_variance}} and 
#'   \code{\link{indicator_skewness}}
#'
#'@export
generic_spews <- function(mat, 
                          subsize = 4,
                          detrend = FALSE,
                          moranI_coarse_grain = FALSE) {
  
  check_mat(mat)
  
  orig_mat <- mat
  
  if ( is.list(mat) ) { 
    results <- lapply(mat, generic_spews, subsize, detrend)
    class(results) <- c('generic_spews', 'generic_spews_list', 
                        'spews_result', 'list')
    return(results)
  }
  
  if (detrend) { 
    mat <- mat - mean(mat)
  }
  
  # Build the right indicator function (closure) depending on whether or not 
  #   moran's I should be computed on coarse-grained matrices.
  if ( moranI_coarse_grain ) { 
    indicf <- function(mat) { 
      mat <- coarse_grain(mat, subsize)
      c(variance = var(as.vector(mat)),
        skewness = abs(moments::skewness(as.vector(mat))),
        moran    = raw_moran(mat),
        mean     = mean(mat))
    }
  } else { 
    indicf <- function(mat) { 
      mat_cg <- coarse_grain(mat, subsize)
      c(variance = var(as.vector(mat_cg)),
        skewness = abs(moments::skewness(as.vector(mat_cg))),
        moran    = raw_moran(mat), # not CG ! 
        mean     = mean(mat_cg))
    }
  }
  
  # Compute the indicators and store the parameters used
  results <- list(results = as.list(indicf(mat)), 
                  orig_data = orig_mat,
                  call = match.call(),
                  subsize = subsize, 
                  indicf  = indicf,
                  detrend = detrend)
  
  class(results) <- c('generic_spews', 'generic_spews_single', 
                      'spews_result', 'list')
  return(results)
}




# ----------------------------
# PRINT METHODS
# ----------------------------

print.generic_spews <- function(obj) { 
  NextMethod("print", obj)
}
print.generic_spews_single <- function(obj) { 
  print( summary.generic_spews_single(obj, null_replicates = 0) )
}
print.generic_spews_list <- function(obj) { 
  print( summary.generic_spews_list(obj, null_replicates = 0) )
}




# ----------------------------
# SUMMARY METHODS
# ----------------------------
# 
#' @title Generic spatial warning signals: summary function
#' 
#' @description Summary and significance testing function for generic 
#'   spatial early-warning signals. 
#' 
#' @param obj A \code{generic_spews} object (as provided by the 
#'   \code{generic_spews} function). 
#' 
#' @param null_replicates The number of replicates to generate in the null 
#'   distribution to test significance
#'   
#' @details This function returns internally a \code{data.frame} of five columns
#'   containing : 
#'   \enumerate{ 
#'     \item \code{value} The indicator value
#'     \item \code{null_mean} The mean of the null distribution
#'     \item \code{null_sd} The standard deviation of the null distribution
#'     \item \code{null_95} The 95%th quantile
#'     \item \code{null_05} The 5%th quantile
#'     \item \code{z_score} The z_score of the observed value in the null 
#'       distribution
#'     \item \code{pval} The p-value (based on the rank of the observed 
#'       value in the null distribution)
#'   }
#' 
#' @seealso \code{\link{plot.generic_spews_summary}}
#' 
#' @examples 
#' data(B)
#' result <- generic_spews( as.binary_matrix(B) )
#' summary(result)
#' 
#'@export
summary.generic_spews <- function(obj, null_replicates = 999) { 
  NextMethod("summary", obj, null_replicates)
}

# Summary function for a single replicate
#'@export
summary.generic_spews_single <- function(obj, null_replicates = 999) { 
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(obj[["orig_data"]],
                                             detrending = obj[["detrend"]], 
                                             nreplicates = null_replicates, 
                                             indicf = obj[["indicf"]])
  
  results <- as.data.frame(null_values) 
  
  # Format output
  indic_list <- c('Variance', 'Skewness', 'Moran\'s I', 'Mean')
  results <- data.frame(indicator = indic_list, results)
  rownames(results) <- indic_list
  
  class(results) <- c('generic_spews_summary', 'spews_summary', 'data.frame')
  results
}

# Summary function for many replicates
#'@export
summary.generic_spews_list <- function(obj, null_replicates = 999) { 
  
  results <- plyr::llply(obj, summary.generic_spews_single, null_replicates)
  results <- lapply(seq_along(results), 
                    function(n) data.frame(replicate = n, results[[n]]))
  results <- do.call(rbind, results)
  
  class(results) <- c('generic_spews_summary', 'spews_summary', 'data.frame')
  return(results)
}




# ----------------------------
# PLOT METHODS
# ----------------------------
# 
#' @title Generic spatial warning signals: plotting function
#' 
#' @description Plot function for generic early warning signals
#' 
#' @param obj A \code{generic_spews} object (as provided by the 
#'   \code{generic_spews} function). 
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. 
#' 
#' @return A ggplot object (usually displayed immediatelly when called at the 
#'   prompt). 
#' 
#' @details Since this function returns a ggplot object, it can be later 
#'   modified to add other graphical elements (e.g. axis names or annotations). 
#' 
#' @seealso \code{\link{generic_spews}}, 
#'   \code{\link{plot.generic_spews_summary}}
# 
#'@export
plot.generic_spews <- function(obj, along = NULL) { 
  if ( 'generic_spews_single' %in% class(obj) ) { 
    stop('I cannot plot a trend with only one value !')
  }
  
  new_data <- summary.generic_spews_list(obj, null_replicates = 0)
  plot.generic_spews_summary(new_data, along, display_null = FALSE)
}

# 
#' @title Plots the results of a generic spatial warning signal summary
#' 
#' @description Plotting functions for generic early warning signals
#' 
#' @param obj A \code{generic_spews_summary} object (as provided by the 
#'   \code{summary} methods for \code{generic_spews} objects). 
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
#' @seealso \code{\link{generic_spews}}, \code{\link{summary.generic_spews}}
#'
#' @examples 
#' data(B)
#' result <- generic_spews( as.binary_matrix(B) )
#' plot(summary(result))
#'
#'@export
plot.generic_spews_summary <- function(obj, 
                                       along = NULL, 
                                       what = 'value',
                                       display_null = TRUE) {  
  
  if ( ! 'replicate' %in% colnames(obj) || 
         !is.null(along) && length(along) <= 1 ) { 
    stop('I cannot plot a trend with only one value')
  }
  
  # If along is not provided, then use the replicate number
  set_default_xlab <- FALSE 
  if ( is.null(along) ) { 
    along <- unique(obj[ ,"replicate"])
    set_default_xlab <- TRUE 
  }
  
  if ( length(unique(obj[ ,'replicate'])) != length(along) ) { 
    stop('External data length (along = ...) does not match ',
         'the number of replicates !')
  }
  
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


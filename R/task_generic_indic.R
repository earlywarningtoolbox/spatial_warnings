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
#' @param discrete Whether the passed data is qualitative or quantitative. If 
#'   the passed data is a binary matrix, then the parameter is set to TRUE. 
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
                          discrete = is.binary_matrix(mat), 
                          subsize = 5,
                          detrend = FALSE) {
  
  orig_mat <- mat
  
  if ( is.list(mat) ) { 
    results <- lapply(mat, with_binmat_check(generic_spews), 
                      subsize, detrend)
    class(results) <- c('generic_spews', 'generic_spews_list', 
                        'spews_result', 'list')
    return(results)
  }
  
  # If it is a binary matrix, then coarse grain it
  if (discrete) { 
    mat <- coarse_grain(mat, subsize = 5)
  }
  
  if (detrend) { 
    mat <- mat - mean(mat)
  }
  
  # Compute the indicators and store the parameters used
  results <- list(results = .generic_spews_core(mat), 
                  orig_data = orig_mat,
                  call = match.call(),
                  discrete = discrete, 
                  subsize = subsize, 
                  detrend = detrend)
  class(results) <- c('generic_spews', 'generic_spews_single', 
                      'spews_result', 'list')
  
  return(results)
}

.generic_spews_core <- function(mat) { 
  list(variance = var(as.vector(mat)),
       skewness = moments::skewness(as.vector(mat)),
       moran    = .moranCpp(mat),
       mean     = mean(mat))
}




# ----------------------------
# PRINT METHODS
# ----------------------------

print.generic_spews_single <- function(obj) { 
  print( summary.generic_spews_single(obj, 0) )
}
print.generic_spews_list <- function(obj) { 
  print( summary.generic_spews_list(obj, 0) )
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
summary.generic_spews_single <- function(obj, null_replicates = 999) { 
  
  results <- data.frame(value = numeric(), null_mean = numeric(),
                        null_sd = numeric(), z_score = numeric(), 
                        pval = numeric())
  
  # Compute a distribution of null values for moran's I
  moran_null <- with(obj,
                     indicator_moran(orig_data, subsize, detrend, 
                                     discrete, null_replicates))
  results <- rbind(results, moran_null)
  
  # Compute pvalues for variance/skewness/
  # Note that this is not yet implemented -> set to zero to not produce 
  #   errors when plotting. 
  results <- rbind(results, 
                   c(obj[['results']][['mean']],     0, 0, 0, 0),
                   c(obj[['results']][['variance']], 0, 0, 0, 0),
                   c(obj[['results']][['skewness']], 0, 0, 0, 0))  
  
  warning('Significance-testing of variance and skewness is not ',
          'implemented yet')
  
  # Format output
  indic_list <- c('Moran\'s I', 'Mean', 'Variance', 'Skewness')
  results <- data.frame(indicator = indic_list, results)
  rownames(results) <- indic_list
  
  class(results) <- c('generic_spews_summary', 'spews_summary', 'data.frame')
  results
}

# Summary function for many replicates
#' @export
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
plot.generic_spews <- function(obj, along) { 
  if ( 'generic_spews_single' %in% class(obj) ) { 
    stop('I cannot plot a trend with only one value !')
  }
  
  new_data <- summary.generic_spews_list(obj, null_replicates = 0)
  plot.generic_spews_summary(new_data, along)
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
                                       along = obj[ ,'replicate'], 
                                       what = 'value',
                                       display_null = TRUE) {  
  
  if ( ! 'replicate' %in% colnames(obj) || length(along) <= 1 ) { 
    stop('I cannot plot a trend with only one value')
  }
  
  if ( max(obj[ ,'replicate']) != length(along) ) { 
    stop('External data (along = ...) does not match ',
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
    
    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes_string(x = 'gradient',
                                                            ymin = 'null_ymin',
                                                            ymax = 'null_ymax'),
                                        data = null_data, 
                                        fill = 'grey',
                                        alpha = .8)
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
  plot <- plot + ggplot2::xlab(as.character(match.call()['along']))
  
  return(plot)
}


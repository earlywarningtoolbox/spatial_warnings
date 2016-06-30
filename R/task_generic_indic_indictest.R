# 
# 
# This file contains the indictest functions for generic spews
# 



#' @title Generic spatial warning signals: significance assessment
#' 
#' @description Significance-testing function for generic spatial early-warning 
#'   signals. 
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
#' @seealso \code{\link{plot.generic_spews_test}}
#' 
#' @examples 
#' data(forestdat)
#' result <- generic_spews(forestdat[['matrices']])
#' indictest(result)
#' 
#'@export
indictest.generic_spews_single <- function(obj, null_replicates = 999, ...) { 
  
  # We do not support low numbers of replicates
  if ( null_replicates < 3 ) { 
    stop('The number of null replicates should be above 3 to ', 
         'assess significance')
  }
  
  # Compute a distribution of null values
  null_values <- compute_indicator_with_null(obj[["orig_data"]],
                                             detrending = obj[["detrend"]], 
                                             nreplicates = null_replicates, 
                                             indicf = obj[["indicf"]])
  
  results <- as.data.frame(null_values) 
  
  # Format output. Note that we always add a "replicate" column even if there 
  # is only one so code works on both
  indic_list <- c('Variance', 'Skewness', 'Moran\'s I', 'Mean')
  results <- data.frame(replicate = 1, 
                        indicator = indic_list, 
                        results)
  rownames(results) <- indic_list
  
  attr(results, 'nreplicates') <- null_replicates
  class(results) <- c('generic_spews_test', 'spews_test', 'data.frame')
  results
}

# Summary function for many replicates
#'@export
indictest.generic_spews_list <- function(obj, null_replicates = 999, ...) { 
  
  results <- plyr::llply(obj, indictest.generic_spews_single, null_replicates, ...)
  
  # Replace replicate column with correct number
  for ( n in seq_along(results) ) { 
    results[[n]][ ,'replicate'] <- n
  }
  results <- do.call(rbind, results)
  
  attr(results, 'nreplicates') <- null_replicates
  class(results) <- c('generic_spews_test', 'spews_test', 'data.frame')
  return(results)
}


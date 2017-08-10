# 
# This file contains a function that identifies patch size distribution types 
#   by fitting all types using ML, then retaining the best one using AIC 
#   (note that it does not perform LR tests across distribution types to 
#   assess significance (yet?))
# 

#' @title Change in patch-size distributions types
#' 
#' @description This functions fits different patch size distributions 
#'  types (power-law, longnormal, exponential and truncated power-law) to 
#'  the patches contained in a matrix. The distributions are returned with 
#'  their corresponding AIC, BIC and AICc to select the best fit.
#'  
#' @param x A logical (TRUE/FALSE values) matrix or a list of these. 
#' 
#' @param xmin The xmin to be used to fit the patch size distributions. Use 
#'   the special values "estimate" to use an estimated xmin for each fit
#' 
#' @param merge If input is a list, then merge all the observed patch-size
#'   distributions in a single one to obtain a better fit.
#' 
#' @param fit_lnorm Fit also a log-normal distribution 
#' 
#' @param xmin_bounds Restrict the possible xmins in this range (defaults to 
#'   the whole range of observed patch sizes)
#' 
#' @param best_by The criterion used to select the best distribution type 
#'   (one of \code{"AIC"}, \code{"BIC"} or \code{"AICc"}). 
#' 
#' @param wrap Detemines whether patches are considered to wraparound when 
#'   reaching one side of the matrix.  
#' 
#' @return A data.frame (or a list of these if x is a list) with the 
#'   following columns:
#'     \itemize{
#'       \item `method` the method used for fitting (currently: only 
#'          log-likelihood is implemented, "ll")
#'       \item `type` the type of distribution fit
#'       \item `npars` the number of parameters of the distribution type
#'       \item `AIC`, `AICc` and `BIC` the values for Akaike Information 
#'         Criterion (or the corrected for small samples equivalent AICc), 
#'         and Bayesion Information Criterion (BIC)
#'       \item `best` A logical vector indicating which distribution is the 
#'         best fit 
#'       \item `expo`, `rate`, `meanlog`, `sdlog` the estimates for distribution
#'         parameters. 
#'       \item 'percolation' A logical value indicating whether there is 
#'         \code{\link{percolation}} in the system. 
#'     }
#' 
#' @references
#' 
#' Kefi, S., Rietkerk, M., Roy, M., Franc, A., de Ruiter, P.C. & Pascual, M.
#' (2011). Robust scaling in ecosystems and the meltdown of patch size
#' distributions before extinction: Patch size distributions towards 
#' extinction. Ecology Letters, 14, 29-35.
#' 
#' Kefi, S., Rietkerk, M., Alados, C.L., Pueyo, Y., Papanastasis, V.P., ElAich,
#' A., et al. (2007). Spatial vegetation patterns and imminent desertification
#' in Mediterranean arid ecosystems. Nature, 449, 213-217.
#' 
#' Clauset et al. #TODO
#' 
#' @examples
#' 
#' data(forestgap)
#' 
#' indicator_psdtype(forestgap)
#'
#'
#'@export 
indicator_psdtype <- function(x, 
                              xmin = 1, 
                              merge = FALSE, 
                              fit_lnorm = FALSE,
                              xmin_bounds = NULL, 
                              best_by = "AIC", 
                              wrap = FALSE) { 
  
  check_mat(x)
  
  if ( !merge && is.list(x) ) { 
    return( lapply(x, indicator_psdtype, xmin, merge, fit_lnorm, xmin_bounds, 
                   best_by, wrap) )
  } 
  
  # Estimate power-law range and set xmin to the estimated value if set to 
  # auto. 
  if ( xmin == "estimate" ) { 
    # Set bounds to search for xmin
    if ( length(psd) > 0 && is.null(xmin_bounds) ) { 
      xmin_bounds <- range(psd)
    }
    xmin <- plrange(psd)['xmin_est']
  }
  
  # Compute psd
  psd <- patchsizes(x, merge = merge, wrap = wrap)
  
  # Compute percolation point. If the user requested a merge of all 
  #   patch size distributions, then we return the proportion of matrices 
  #   with percolation. 
  if ( is.list(x) ) { 
    percol <- lapply(x, percolation)
    percol <- mean(unlist(percol))
    percol_empty <- lapply(x, function(x) percolation(!x))
    percol_empty <- mean(unlist(percol_empty))
  } else { 
    percol <- percolation(x)
    percol_empty <- percolation(!x)
  } 
  
  psdtype_result <- psdtype(psd, xmin, best_by, fit_lnorm)
  
  result <- data.frame(psdtype_result, 
                       percolation = percol, 
                       percolation_empty = percol_empty)
  return(result)
}

psdtype <- function(psd, xmin, best_by, fit_lnorm) { 
  
  table_names <- c('method', 'type', 'npars', 'AIC', 'AICc', 'BIC', 'best', 
                   'expo', 'rate', 'xmin_fit')
  if ( fit_lnorm ) { 
    table_names <- c(table_names, 'meanlog', 'sdlog')
  }
  
  # Chop off psd to what is above xmin 
  psd <- psd[psd >= xmin]
  
  # We return NA if there are not enough patches to work with 
  if ( length(unique(psd)) <= 3 ) { 
    NA_result <- as.data.frame(as.list(rep(NA, length(table_names))))
    names(NA_result) <- table_names
    return(NA_result)
  }
  
  # Fit a model for everyone. Note that we store the results of the variables 
  # here so we reuse the previous expo and rate for tpl fitting. 
  plfit  <- pl_fit(psd, xmin)
  expfit <- exp_fit(psd, xmin)
  tplfit <- tpl_fit(psd, xmin)
  
  models <- list(pl = plfit, tpl = tplfit, exp = expfit)
  
  if (fit_lnorm) { 
    lnormfit <- lnorm_fit(psd, xmin)
    models <- c(models, list(lnorm = lnormfit))
  }
  
  models <- lapply(models, as.data.frame)
  models <- do.call(rbind.fill, models)
  row.names(models) <- models[ ,'type']
  
  # Compute AICs
  models[ ,'AIC']  <- get_AIC(models[ ,'ll'],  models[ ,'npars'])
  models[ ,'AICc'] <- get_AICc(models[ ,'ll'], models[ ,'npars'], length(psd))
  models[ ,'BIC']  <- get_BIC(models[ ,'ll'],  models[ ,'npars'], length(psd))
  
  # We need to remove NA's here as sometimes one of the fits fails and its "best"
  # column is NA. We do not consider failed fit as good solutions. 
  models[ ,'best'] <- models[ ,best_by] == min(models[ ,best_by], na.rm = TRUE)
  models[ ,'best'] <- models[ ,'best'] & ! is.na(models[ ,'best']) 
  
  # Add an xmin column 
  models[ ,"xmin_fit"] <- xmin
  
  # Reorganize columns 
  models <- models[ ,table_names]
  
  return(models)
}

get_AIC <- function(ll, k) { 
  2*k - 2*ll 
}

get_AICc <- function(ll, k, n) { 
  2*k - 2*ll + (2*k*(k+1))/(n-k-1)
}

get_BIC <- function(ll, k, n) { 
  2*k*log(n) - 2*ll 
}

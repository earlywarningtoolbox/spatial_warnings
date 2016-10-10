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
#' @param input A logical (TRUE/FALSE values) matrix or a list of these. 
#' 
#' @param best_by The criterion used to select the best distribution type 
#'   (one of \code{"AIC"}, \code{"BIC"} or \code{"AICc"}). 
#' 
#' @param merge If input is a list, then merge all the observed patch-size
#'   distributions in a single one to obtain a better fit.
#' 
#' @return A data.frame (or a list of these if input was a list) with the 
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
#' Kéfi, S., Rietkerk, M., Roy, M., Franc, A., de Ruiter, P.C. & Pascual, M.
#' (2011). Robust scaling in ecosystems and the meltdown of patch size
#' distributions before extinction: Patch size distributions towards 
#' extinction. Ecology Letters, 14, 29–35.
#' 
#' Kéfi, S., Rietkerk, M., Alados, C.L., Pueyo, Y., Papanastasis, V.P., ElAich,
#' A., et al. (2007). Spatial vegetation patterns and imminent desertification
#' in Mediterranean arid ecosystems. Nature, 449, 213–217.
#' 
#' Clauset et al. #TODO
#' 
#' @examples
#' 
#' data(arid)
#' 
#' # Computing patch-size distributions can only work on binary (TRUE/FALSE)
#' #   data: we threshold the aerial images first. 
#' arid.bw <- arid[[1]] > mean(arid[[1]])
#' indicator_psdtype(arid.bw)
#'
#'
#'@export 
indicator_psdtype <- function(input, merge = FALSE, best_by = "AIC") { 
  
  check_mat(input)
  
  if ( !merge && is.list(input) ) { 
    return( lapply(input, indicator_psdtype, best_by = best_by) )
  } 
  
  multiple_matrices <- is.list(input)
  
  # Compute psd
  psd <- patchsizes(input, merge = merge)
  
  # Compute percolation point. If the user requested a merge of all 
  #   patch size distributions, then we return the proportion of matrices 
  #   with percolation. 
  if ( multiple_matrices ) { 
    perc <- unlist( lapply(input, percolation) )
    perc <- mean(perc) # Compute average percolation 
  } else { 
    perc <- percolation(input)
  }
  
  result <- data.frame(psdtype(psd, best_by = "AIC"), 
                       percolation = perc)
  return(result)
}

psdtype <- function(psd, best_by = "AIC") { 
  
  table_names <- c('method', 'type', 'npars', 'AIC', 'AICc', 'BIC', 'best', 
                'expo', 'rate', 'meanlog', 'sdlog')
  
  # If there are not enough patches to work with -> return NA early
  if ( length(unique(psd)) <= 2 ) { 
    warning('Not enough different patch sizes to fit distribution: returning NA')
    NAresult <- as.data.frame(as.list(rep(NA, length(table_names))))
    colnames(NAresult) <- table_names
    return(NAresult)
  }
  
  # Fit a model for everyone
  models <- list(pl  =   pl_fit(psd), 
                 tpl =   tpl_fit(psd), 
                 exp =   exp_fit(psd), 
                 lnorm = lnorm_fit(psd)) 
  
  models <- lapply(models, as.data.frame)
  models <- do.call(rbind.fill, models)
  row.names(models) <- models[ ,'type']
  
  # Compute AICs
  models[ ,'AIC']  <- get_AIC(models[ ,'ll'],  models[ ,'npars'])
  models[ ,'AICc'] <- get_AICc(models[ ,'ll'], models[ ,'npars'], length(psd))
  models[ ,'BIC'] <- get_BIC(models[ ,'ll'],  models[ ,'npars'], length(psd))
  
  models[ ,'best'] <- models[ ,best_by] == min(models[ ,best_by])
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

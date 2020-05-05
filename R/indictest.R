# 
# Indictest is a method for testing the significance of spatial indicators 
# based on shuffling the original matrix. 
# 
# This file only defines the global method and maybe some helpers. For each 
#   method, please refer to the task_* files. 

#' @title Significance-assessment of spatial early-warning signals
#' 
#' @description Assess the significance of spatial early-warning indicators
#' 
#' @param x A spatial warning object such as one produced by the \code{*_sews}
#'   functions, or \code{\link{compute_indicator}}
#' 
#' @param nulln The number of values to compute to produce the null 
#'   distribution 
#' 
#' @param null_method The method used to produce the null values (right now 
#'   only the permutation method is supported, 'perm')
#' 
#' @param null_control List of arguments used to control the generation of 
#'   null matrices. If NULL, then arguments then sensible defaults are chosen 
#'   (see Details)
#' 
#' @param ... Additional arguments are ignored
#' 
#' @return An object with a class ending in \code{*_sews_test}, whose exact
#'   class depends on the input object. \code{plot}, \code{summary} methods are
#'   available to display the results of computations, and additional methods
#'   may be available depending on the input object (e.g. see
#'   \link{patchdistr_sews_plot}). 
#' 
#' @details 
#' 
#' \code{indictest} is used to test the significance of early-warning signals
#'   against 'null matrices', which represent the expected spatial structure 
#'   in the absence of the biological process of interest. 
#' 
#' For a given indicator, a null distribution is obtained by producing a set 
#'   of 'null' matrices on which indicator values are recomputed. This produces 
#'   a null distribution of \code{nulln} values against which the observed
#'   value is tested. 
#' 
#' Several methods are available to produce the set of null matrices. If 
#'   \code{null_method} is set ot "perm", the original matrix is reshuffled 
#'   to obtain a null matrix. If the method is "binom", the mean cover of a 
#'   matrix \deqn{mu} is computed, then the matrix is filled randomly with 
#'   TRUE values (with probability \deqn{mu}) and FALSE (with probability 
#'   \deqn{1 - mu}). This method only works with logical matrices (with 
#'   TRUE/FALSE values). 
#' 
#' \code{null_method} can be set to 'smooth'. In that case, an isotropic spline 
#'   will be fitted to the matrix (using \link[mgcv]{gam}), and values will 
#'   be 
#' 
#' @seealso \code{\link{generic_sews}}, \code{\link{spectral_sews}}, 
#'   \code{\link{kbdm_sews}},
#'   \code{\link{compute_indicator}}
#' 
#' @references 
#' 
#' Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina, 
#' V.N., et al. (2014). Early Warning Signals of Ecological Transitions: 
#' Methods for Spatial Patterns. PLoS ONE, 9, e92097
#' 
#'@export
indictest <- function(x, 
                      nulln = 999, 
                      null_method = 'perm', 
                      null_control = NULL, 
                      ...) { 
  # If indictest has already run on the object, return itself
  if ( any(grepl("_sews_test_", class(x))) ) { 
    return(x)
  }
  
  UseMethod('indictest')
}


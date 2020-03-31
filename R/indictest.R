# 
# Indictest is a method for testing the significance of spatial indicators 
# based on shuffling the original matrix. 
# 
# This file only defines the global method and maybe some helpers. For each 
#   method, please refer to the task_* files. 

#' @title Significance-assessment of spatial early-warning signals
#' 
#' @description Assess the significance of spatial early-warning values
#' 
#' @param x A spatial warning object such as one produced by the \code{*_sews} 
#'   function family
#' 
#' @param nperm The number of permutations to carry out to produce the null 
#'   distribution 
#' 
#' @param null_method The method used to test significance (right now 
#'   only the permutation method is supported, 'perm')
#' 
#' @param ... Additional arguments passed to methods 
#' 
#' @return An object of class ending in \code{*_sews_test}, whose exact class 
#'   depends on the input object (in reality a data.frame)
#' 
#' @details 
#' 
#' \code{indictest} is used to test the significance of early-warning signals
#'   against 'null matrices', which represent the expected spatial structure 
#'   in the absence of the biological process of interest. This is done based 
#'   on the following procedure: (1) a set of N null matrices is 
#'   generated (this number is set by the argument \code{nperm}); (2) indicator
#'   values are recomputed on this set of null matrices and (3) the significance 
#'   of the observed indicator value is tested against this distribution. 
#'   Several methods are available to produce the set of null matrices. 
#'   
#' The first option is to generate them by reshuffling the original 
#'   observed matrix. This produces null matrices with the same exact average 
#'   value, but with a random spatial structure (no autocorrelation at all 
#'   lags). Use \code{null_method = "perm"} to use this option. 
#' 
#' A second option is to use random draws from ... 
#' 
#' @seealso \code{\link{generic_sews}}, \code{\link{spectral_sews}}
#'   
#' @references 
#' 
#'   Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina, 
#'   V.N., et al. (2014). Early Warning Signals of Ecological Transitions: 
#'   Methods for Spatial Patterns. PLoS ONE, 9, e92097
#' 
#'@export
indictest <- function(x, 
                      nperm = 999, 
                      null_method = 'perm', 
                      ...) { 
  UseMethod('indictest')
}


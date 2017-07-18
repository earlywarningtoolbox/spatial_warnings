# 
# Indictest is a method for testing the significance of spatial indicators. 
# 
# This file only defines the global method and maybe some helpers. For each 
#   method, please refer to the task_* files. 

#' @title Significance-assessment of spatial early-warning signals
#' 
#' @description Assess the significance of spatial early-warning values
#' 
#' @param obj A spatial warning object such as one produced by the *_spews 
#'   function family
#' 
#' @param nperm How many times should the input matrix be randomized
#'   to create the null distribution ?
#' 
#' @param ... Additionnal arguments passed to methods 
#' 
#' @return An object of class \code{*_spews_test} depending on the input object
#'   (actually, a data.frame)
#'
#' @details 
#' 
#' The significance of many early-warning signals can be estimated by 
#'   reshuffling the original matrix. Indicators are then recomputed 
#'   on the shuffled matrices and the values obtained are used as a null 
#'   distribution. P-values are obtained based on the rank of the observered
#'   value in the null distribution. 
#' 
#' @seealso \code{\link{generic_spews}}, \code{\link{spectral_spews}}
#'   
#' @references 
#'   Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina, 
#'   V.N., et al. (2014). Early Warning Signals of Ecological Transitions: 
#'   Methods for Spatial Patterns. PLoS ONE, 9, e92097
# 
# 
# Define global method
#'@export
indictest <- function(obj, nperm = 999, ...) { 
  UseMethod('indictest')
}


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
#' @param x A spatial warning object such as one produced by the \code{*_spews} 
#'   function family
#' 
#' @param nperm The number of permutations to carry out to produce the null 
#'   distribution 
#' 
#' @param ... Additional arguments passed to methods 
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
#' 
#'   Kefi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina, 
#'   V.N., et al. (2014). Early Warning Signals of Ecological Transitions: 
#'   Methods for Spatial Patterns. PLoS ONE, 9, e92097
#'  
#'@export
indictest <- function(x, nperm = 999, ...) { 
  UseMethod('indictest')
}


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
#' @param null_replicates How many times should the input matrix be randomized
#'   to create the null distribution ?
#'   
#' @return An object of class \code{*_spews_test} depending on the input object
#'   (actually, a data.frame)
#'
#' @details This function implements the methods described in Kéfi et al. 
#'   (2014) that suggest randomizing the spatial matrix to compute a null 
#'   distribution. The observed values of indicators are tested against 
#'   this distribution to obtain p-values. 
#'   
#' @references 
#'   Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina, 
#'   V.N., et al. (2014). Early Warning Signals of Ecological Transitions: 
#'   Methods for Spatial Patterns. PLoS ONE, 9, e92097

# Define global method
#'@export
indictest <- function(obj, null_replicates = 999, ...) { 
  UseMethod('indictest')
}


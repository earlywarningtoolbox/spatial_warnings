# 
#
#' @title Custom Spatial Early-Warning signals
#' 
#' @description Computation, significance assessment and display of trends 
#'   of a custom, user-defined indicator.
#' 
#' @param fun A function that takes a real-valued matrix as input and returns 
#'   a single, numerical value. 
#' 
#' @param indicname The indicator name. Optional, used for plots and textual 
#'   summaries, but mandatory if \code{fun} is an anonymous function. 
#' 
#' @return 
#' 
#' \code{create_indicator} returns a function that can be used in the same way 
#'   than the other \code{*_sews} functions (e.g. \code{generic_sews})
#' 
#' @details 
#' 
#' Spatial Early-warning signals (EWS) are metrics that are based on the 
#'   spatial structure of a system and measure the degradation of an ecological 
#'   system. The package \code{spatialwarnings} provides 
#'   generic indicators (\code{\link{generic_sews}}), spectrum-based 
#'   indicators (\code{\link{spectral_sews}}) and indicators based on patch 
#'   size distributions (\code{\link{patchdistr_sews}}). 
#'   
#' \code{create_indicator} extends the package to any arbitrary function. 
#'   It takes a function `fun` and returns another function that can be used 
#'   as an indicator similar to the \code{*_sews} function family. The 
#'   results of this function can be assessed for significance using 
#'   \code{indictest} and trends can be displayed using 
#'   \code{plot}, \code{summary}, etc. (see Examples). \code{custom_indicator} 
#'   does the same but without creating an intermediate indicator function. 
#' 
#' \code{fun} should be a function that takes as input a matrix and possibly
#'   more arguments, and return a single numeric value. Note that the matrix 
#'   is converted internally to numeric values, as a side effect of using 
#'   c++ code when assessing significance. When working with logical matrices 
#'   (e.g. when computing patch size distributions), the matrix has to be 
#'   explicitly converted to logical within function `fun`. 
#' 
#' @examples
#' 
#' # Use the maximum patch size as indicator of degradation
#' maxpatchsize <- function(mat) { 
#'   # Note that we explicitely convert mat here to logical as it can be 
#'   # transformed into numeric internally. 
#'   max(patchsizes(mat > 0))
#' }
#' 
#' # Create the indicator function
#' maxpatch_sews <- create_indicator(maxpatchsize)
#' 
#' # Then work with this function as if it were a function from the *_sews 
#' # family. 
#' mp_indic <- maxpatch_sews(forestgap)
#' summary(mp_indic)
#' 
#' \dontrun{ 
#' # Assess significance and display trends
#' options(mc.cores = 2)
#' mp_test <- indictest(mp_indic, nperm = 49)
#' plot(mp_test)
#' }
#' 
#' 
#' 
#' # Try spatial coefficient of variation as a spatial EWS. This function can 
#' # have arguments. 
#' spatial_cv <- function(mat, subsize) { 
#'   matc <- coarse_grain(mat, subsize)
#'   return( sd(matc) / mean(matc) )
#' }
#' 
#' # Create indicator function
#' cv_sews <- create_indicator(spatial_cv)
#' 
#' # Compute and display trends
#' cv_indic <- cv_sews(serengeti, subsize = 3)
#' plot(cv_indic, along = serengeti.rain)
#' 
#' # We can do the same work in one run using custom_indicator
#' cv_indic2 <- custom_indicator(serengeti, spatial_cv, subsize = 3)
#' plot(cv_indic2, along = serengeti.rain)
#' 
#' \dontrun{ 
#' indictest(cv_indic, nperm = 99)
#' }
#'@export
create_indicator <- function(fun, 
                             indicname = as.character(substitute(fun))) { 
  
  # Test if indicname is derived from an anonymous function
  if ( length(indicname) > 1 ) { 
    warning('A valid name could not be derived from the function passed to create_indicator,\n', 
            'using the default name custom_indic')
    indicname <- "custom_indic"
  }
  
  # Subfunction that works only on a matrix
  get_one_result <- function(mat, ...) { 
    result <- list(value     = fun(mat, ...), 
                   orig_data = mat, 
                   fun.args  = as.list(match.call(expand.dots = FALSE))[['...']], 
                   indicf = fun)
    
    class(result) <- c('custom_sews_single', 'simple_sews_single', 'custom_sews', 'list')
    attr(result, 'indicname') <- indicname
    return(result)
  }
  
  # Actual function produced
  function(mat, ...) { 
    if ( is.list(mat) ) { 
      result <- parallel::mclapply(mat, get_one_result, ...)
      names(result) <- names(mat)
      class(result) <- c('custom_sews_single', 'simple_sews_list', 'custom_sews', 'list')
      attr(result, 'indicname') <- indicname
    } else { 
      result <- get_one_result(mat, ...)
    }
    return(result)
  }
  
}


#
#' @rdname create_indicator
#'
#' @param mat A matrix or a list of matrices. 
#' 
#'@export
custom_indicator <- function(mat, fun, 
                             indicname = as.character(substitute(fun)), 
                             ...) { 
  indicfun <- create_indicator(fun, indicname = indicname)
  indicfun(mat, ...)
}

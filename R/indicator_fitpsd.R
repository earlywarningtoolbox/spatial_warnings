# 
#' @title fit candidate cumulative patch size distribution functions.
#'
#' @param x A patch size vector or a list of patch size vectors.
#'
#' @return A list object of class 'psdfit' containing the pooled cumulative 
#'   patch size distribution data, as well as the AICs and model outputs of the 
#'   candidate models.
#' 
#' @details A S3 method for \code{summary()} does exist and only returns the 
#'   model summary of the most parsimonious model. 
#' 
#' @examples
#' 
#' data(B)
#' indicator_fitpsd(B)
#' 
#' @export

indicator_fitpsd <- function(x = NULL, cumpsd = indicator_cumpsd(x)) {
  check_mat(x) # sanity checks for the passed matrix or list
  
  if ( ! is.null(x) && is.list(x)) { # FALSE for x = NULL
    return( lapply(x, indicator_fitpsd) ) 
  } else {
    
    out <- list()   # prepare output object
    
    out$psd <- cumpsd   # get cumulative patch size distributions
    out$best <- NA    # prepare for out
    out$AIC <- vector("numeric", length = 3)  # prepare for out
    names(out$AIC) <- c("TPLdown", "PL", "TPLup" )
    out$dAIC <- vector("numeric", length = 3)  # prepare for out
    names(out$dAIC) <- c("TPLdown", "PL", "TPLup" )
    # fit linear power law model as starting value for parameter estimation
    PLlm <- lm(I(log(p)) ~  1 - I(log(size)) , data = out$psd) 
    
    # try to fit truncated power law
    
    try( {out$TPLdown <- nls(I(log(p)) ~ I( alpha*log(size)-size*Sx ),
                             data = out$psd,
                             start = list(alpha =  PLlm$coefficients, Sx = 1/200),
                             #algorithm = "port",
                             trace = FALSE
    )}, silent = TRUE)
    
    if (!is.null(out$TPLdown) && !coefficients(out$TPLdown)["Sx"] <= 0 ) {
      out$AIC[1] <- AIC(out$TPLdown) 
    } else {
      out$TPLdown <- list(NA)
      out$AIC[1] <- NA
    }
    
    # try to fit straight power law
    
    try({out$PL <- nls(I(log(p)) ~ alpha * log(size), 
                       data = out$psd,
                       start = list( alpha =  PLlm$coefficients ),
                       trace = FALSE,
                       nls.control(maxiter = 50)
    )}, silent = TRUE)
    
    if(!is.null(out$PL)) {
      out$AIC[2] <- AIC(out$PL)
    } else {
      out$PL  <- list(NA)
      out$AIC[2] <- NA
    }
    
    # try to fit up-bent power law
    
    try({out$TPLup <- nls(I(log(p)) ~  log(b) + log(1+(size^(alpha))/b ) , 
                          data = out$psd,
                          start = list( alpha =  PLlm$coefficients, b = p_spanning ) , 
                          nls.control(maxiter = 50)
    )}, silent = TRUE
    )
    
    
    if(!is.null(out$TPLup)) {
      out$AIC[3] <- AIC(out$TPLup) 
    } else { 
      #result$fit$summary$TPLup  <- list(NA)
      out$TPLup  <- list(NA)
      out$AIC[3] <- NA
    }
    
    # compare models
    
    out$dAIC <-   out$AIC -min(out$AIC, na.rm = TRUE)
    
    out$best <- which.min(out$AIC)
    
    class(out) <- "psdfit"
    return(out)
  }
} 


#' @export

plot.psdfit <- function(x, ...) {
  plot(p ~ size, x$psd, log = "xy", ...)
  model <- x[[names(x$best)]]
  x_new <- seq(1, max(x$psd$size), length = 64 )
  y_new <- predict(model, newdata = list(size = x_new))
  lines(x_new, exp(y_new))
}


#' @export

summary.psdfit <- function(x)   summary(x[[x$best+4]])

plot.psdfit <- function(x, ...) {
  
  plot(p ~ size, x$psd, log = "xy")
  s_new <- seq(1, max(x$psd$size), length = 100) 
  p_new <- exp(predict(x[[x$best+4]], newdata = list(size = s_new)))
  lines(p_new ~ s_new)
} 

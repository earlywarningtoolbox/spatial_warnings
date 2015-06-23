#' Get cumulative patch sizes.
#'
#' @param x A vector of patch sizes or a list of vectors.
#'
#' @return A dataframe of the unique patch sizes \code{size}, the number of patches of equal or larger size than each value in \code{size}, and a probability that any given patch is equal or larger size than each value in \code{size}. 
#' @export
#' 
psd <- function(x) {
  
  if(class(x) == "list") { 
      lapply(x, psd)
    } else {
    out <- data.frame(size =  unique(x))
      out$n = sapply(1:length(out$size), function(i)  length(which(x >= out$size[i])) ) 
      out$p = out$n/length(x)
      
      return(out)
  }
}


#' fit candidate cumulative patch size distribution functions.
#'
#' @param x A patch size vector or a list of patch size vectors.
#'
#' @return A list object of class 'psdfit' containing the pooled cumulative patch size distribution data, as well as the AICs and model outputs of the candidate models.
#' 
#' @details A S3 method for \code{summary()} does exist and only returns the model summary of the most parsimonious model. 
#' 
#' @export
#' 

fitpsd <- function(x) {
  
  
  out <- list()   # prepare output object
  
  out$psd <- psd(x)   # get cumulative patch size distributions
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
  )}, silent = TRUE
  )    
  
  if(!is.null(out$TPLdown) & !coefficients(out$TPLdown)["Sx"] <= 0 ) {
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
  )}, silent = TRUE
  )
  
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



summary.psdfit <- function(x)   summary(x[[x$best+4]])
  
plot.psdfit <- function(x, ...) {
  
  plot(p ~ size, x$psd, log = "xy")
  s_new <- seq(1, max(x$psd$size), length = 100) 
  p_new <- exp(predict(x[[x$best+4]], newdata = list(size = s_new)))
  lines(p_new ~ s_new)
} 

# 
#' @title fit candidate cumulative patch size distribution functions.
#'
#' @param x A binary matrix or a list of binary matrices
#' 
#' @param cumpsd A cumulative patch size distribution as returned by 
#'   indicator_cumpsd
#' 
#' @references Kefi, S., Rietkerk, M., Alados, C. L., Pueyo, Y., Papanastasis, 
#' V. P., ElAich, A., & De Ruiter, P. C. (2007). Spatial vegetation patterns 
#' and imminent desertification in Mediterranean arid ecosystems. 
#' Nature, 449(7159), 213-217.
#' 
#' @references Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). 
#' Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
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
indicator_fitpsd <- function(x = NULL, 
                             cumpsd = indicator_cumpsd(x)) {
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
    
    if( all(is.na(out$AIC)) ) {
      out$dAIC <- out$AIC
    } else {
      out$dAIC <-  out$AIC - min(out$AIC, na.rm = TRUE)
    }
    
    out$best <- which.min(out$AIC)
    if(length(out$best) == 0) out$best <- c(None = NA)
    
    class(out) <- c("psdfit","list")
    return(out)
  }
} 

#' @title Plot a `psdfit` object
#' 
#' @param x A `psdfit` object as returned by `indicator_fitpsd`
#'
#' @param all.models Should all fitted models be displayed on the plot ?
#' 
#' @examples 
#' data(B)
#' fit <- indicator_fitpsd(B)
#' plot(B)
#' 
#' @export 
plot.psdfit <- function(x, all.models = FALSE) {
  # Get the base plot of the cumulative psd
  baseplot <- plot.indicator_cumpsd(x$psd, add.line = FALSE) 
  
  x_new <- seq(1, max(x$psd$size), length = 64 )
  
  # Compute all fitted models results
  model.dat <- data.frame(model = factor(), x = numeric(), y = numeric())
  for (model in c("PL", "TPLdown", "TPLup", "EXP")) { 
    # We test whether it is a reasonable model object
    # NB: Another idea would be to test for an existing predict() method if 
    # fitting functions spits objects of classes other than nls ?
    if ( inherits(x[[model]], 'nls') ) { 
      y_new <- predict(x[[model]], newdata = list(size = x_new))
      # Format data for ggplot
      model.dat <- rbind(model.dat, data.frame(model = model, x = x_new, 
                                               y = exp(y_new)))
    }
  }
  
  # Discard data if we want only the best model
  if ( !all.models ) { 
    model.dat <- model.dat[ model.dat[ ,'model'] == names(x$best), ]
  }
  
  # Warn if nothing could be fitted
  if ( nrow(model.dat) == 0 ) { 
    warning("No model could be fitted to the data\n") 
  # Or else add the fit data to the plot
  } else { 
    baseplot <- baseplot + 
                  ggplot2::geom_path(ggplot2::aes(x = x, y = y, color = model),
                                    data = model.dat)
  }
  
  return(baseplot)
}


#' @export
summary.psdfit <- function(x, print.all = FALSE)  {
  cat('Patch-size distribution fitting results:\n')
  
  if (print.all) { 
    return( lapply(x, summary) )
  } else if (names(x$best) %in% c("PL", "TPLdown", "TPLup", "EXP")) { 
    cat('Best model:')
    return( summary(x[[x$best+4]]) )
  } else { 
    cat("No model could be fitted to the data\n") 
  }
  
}

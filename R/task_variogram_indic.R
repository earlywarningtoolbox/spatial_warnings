# 
# Variogram-based indicators. 
# 
#' @title Early-Warning signals based on variograms 
#' 
#' @description Compute Early-warning signals based on metrics derived 
#'   form semi-variograms. 
#' 
#' @param mat A matrix (TRUE/FALSE values) or a list of matrices 
#' 
#' @param subset_frac The number of points to consider in the matrix (as a 
#'   proportion of the total number of cells) used to computed the variogram. 
#'   A lower value here will speed up computations, at the expense of precision.
#' 
#' @param model The variogram model to use, either "sph" (for a spherical model)
#'   or "exp" (for an exponential model)
#' 
#' @return A list object of class "variogram_sews", that can be displayed 
#'   using \code{summary()}, \code{plot()}, etc. Significance of values can 
#'   be tested using \code{\link{indictest}}. 
#' 
#' @details 
#' 
#' During ecosystem degradation and especially before a collapse occurs in 
#'   some ecosystems, autocorrelation is expected to increase in a landscape. 
#'   This increase can be measured based on variograms, which represent how 
#'   the difference (variance) between two points in a landscape varies as 
#'   a function of distance. 
#' 
#' The approach used to derive variogram-based EWS is to compute the 
#'   empirical variogram of a landscape (represented passed as a matrix of 
#'   values), then fit a variogram model to it. Three 
#'   parameters are then extracted from the variogram model (see Nijp et al.
#'   2019 for a visual description of these parameters): 
#'   \enumerate{ 
#'     \item The \emph{nugget} (intercept)
#'     \item The \emph{partial sill}, i.e. the reduction in semivariance at 
#'       distance zero
#'     \item The \emph{correlation range}, i.e. the distance at which the 
#'       relationship between semivariance and distance becomes flat
#'   }
#' 
#' Additionnally, the \emph{structural variance} is computed as 
#'   (partial sill)/(nugget + partial sill), wich quantifies whether the 
#'   data are spatially structured (structural variance of one), or completely 
#'   unstructured (value of zero).
#' 
#' Theoretical work suggests that partial sill, correlation range and 
#'   structural variance should increase before a regime shift occurs in 
#'   an ecosystem (Nijp et al. 2019). 
#' 
#' @seealso \
#'   \code{\link[=variogram_sews_plot]{plot}}, 
#'   \code{\link[=variogram_sews_plot]{plot_variogram}}, 
#'   \code{\link[=variogram_sews_predict]{predict}}, 
#'   \code{\link{indictest}}
#' 
#' @references 
#' 
#' Nijp, Jelmer J., Arnaud J.A.M. Temme, George A.K. Voorn, Lammert Kooistra,
#'   Geerten M. Hengeveld, Merel B. Soons, Adriaan J. Teuling, and Jakob
#'   Wallinga. (2019) Spatial Early Warning Signals for Impending Regime Shifts:
#'   A Practical Framework for Application in Real-world Landscapes. Global
#'   Change Biology 25 (6): 1905-21. <doi:10.1111/gcb.14591>
#'
#' @examples 
#' 
#' \dontrun{
#' serengeti_ews <- variogram_sews(serengeti, subset_frac = 0.01, 
#'                                 model ="exp")
#' plot(serengeti_ews)
#' summary(serengeti_ews)
#' 
#' # nulln should be set to a higher values for meaningful results
#' serengeti_test <- indictest(serengeti_ews, nulln = 9)
#' plot(serengeti_test) # gray ribbons indicate the null indicator values 
#' summary(serengeti_test)
#' }
#' 
#'@export
variogram_sews <- function(mat, 
                           subset_frac = 0.05, 
                           model = "sph") { 
  
  if ( ! model %in% c("sph", "exp") ) { 
    stop(paste0("The variogram model must be one of 'sph' (spherical model) ", 
                "or 'exp' (exponential model)"))
  }
  
  # Handle list case
  if ( is.list(mat) ) { 
    results <- lapply(mat, variogram_sews, 
                      subset_frac, model)
    names(results) <- names(mat)
    class(results) <- c('variogram_sews_list',  'variogram_sews', 'list')
    attr(results, 'indicname') <- "Variogram-based indicators"
    return(results)
  }
  
  # Returns a list with components 'vario' (the empirical variogram) and 
  # 'fit' (its fit)
  vario <- fit_variogram(mat, subset_frac, model = model)
  metrics <- compute_vario_metrics(vario[['pars']])
  
  # Return list containing both
  output <- list(variogram = vario[['vario']], 
                 fit = vario[['pars']], 
                 metrics = metrics, 
                 orig_data = mat, 
                 pars = list(subset_frac = subset_frac, 
                             model = model), 
                 # Locations at which the variogram was sampled
                 locations = vario[["locations"]], 
                 call = match.call())
  class(output) <- c('variogram_sews_single', 'variogram_sews', 'list')
  attr(output, 'indicname') <- "Variogram-based indicators"
  
  return(output)
}


# Methods 
#'@export
summary.variogram_sews_list <- function(object, ...) { 
  
  cat('Spatial Early-Warning:', attr(object, "indicname"), '\n') 
  cat('\n')
  display_size_info(object)
  cat('\n')
  
  summary.tab <- as.data.frame(object)
  summary.tab <- reshape2::dcast(summary.tab, 
                                 matrixn ~ indic, value.var = "value")
  names(summary.tab)[1] <- c('Mat. #')
  print.data.frame(summary.tab, row.names = FALSE, digits = DIGITS)
  
  cat('\n')
  cat(paste0('Use as.data.frame() or extract_variogram() to retrieve values ', 
             'in a convenient form\n'))
  
  invisible(summary.tab)
}
#'@export
summary.variogram_sews_single <- function(object, ...) { 
  object_list <- list(object)
  attr(object_list, "indicname") <- attr(object, "indicname")
  summary.variogram_sews_list(object_list)
}

#'@export
print.variogram_sews_list <- function(x, ...) { 
  summary.variogram_sews_list(x, ...)
}
#'@export
print.variogram_sews_single <- function(x, ...) { 
  summary.variogram_sews_single(x, ...)
}

#'@export
as.data.frame.variogram_sews_single <- function(x, ...) { 
  as.data.frame.variogram_sews_list(list(x))
}
#'@export
as.data.frame.variogram_sews_list <- function(x, ...) { 
  x <- lapply(x, function(o) { 
    o[["value"]] <- o[["metrics"]]
    o
  })
  as.data.frame.simple_sews_list(x, ...)
}

# Extract the variogram as a data frame from a variogram_sews object
#' 
#' @title extract_variogram() method for variogram_sews objects
#' 
#' @description Extract the empirical variogram from a \code{variogram_sews} 
#'   object
#' 
#' @param x An object produced by \code{variogram_sews}, or
#'   \code{indictest}
#' 
#' @param ... Additional arguments (ignored)
#' 
#' @return A data.frame containing the variogram with the distances 
#'   (column \code{dist}), the empirical semivariance values (\code{gamma}), 
#'   and if object contains more than one matrix, a column named \code{matrixn}. 
#' 
#' @examples 
#' 
#' \dontrun{ 
#' vario_indics <- variogram_sews(serengeti)
#' predict(vario_indics)
#' vario_test <- indictest(vario_indics, nulln = 19)
#' predict(vario_test) # same result
#' }
#' 
#' @seealso \code{\link{variogram_sews}}, 
#'   \code{\link[=variogram_sews_plot]{plot}}, 
#'   \code{\link[=variogram_sews_plot]{plot_variogram}}, 
#'   \code{\link[=variogram_sews_predict]{predict}}, 
#'   \code{\link{extract_variogram}}, 
#' 
#'@export 
extract_variogram <- function(x, ...) { 
  UseMethod("extract_variogram")
}
#'@method extract_variogram variogram_sews_list
#'@export
extract_variogram.variogram_sews_list <- function(x, ...) { 
  all_varios <- Map(function(n, o) { 
    data.frame(matrixn = n, o[["variogram"]]) 
  }, n = seq_along(x), o = x)
  do.call(rbind, all_varios)
}
#'@method extract_variogram variogram_sews_single
#'@export
extract_variogram.variogram_sews_single <- function(x, ...) { 
  data.frame(matrixn = 1, x[["variogram"]]) 
}



# Predict methods 
# --------------------------------------------------
#' @rdname variogram_sews_predict
#' @name variogram_sews_predict
#' 
#' @title predict() method for variogram_sews objects
#' 
#' @description Export the fitted variogram(s)
#' 
#' @param object An object produced by variogram_sews 
#' 
#' @param newdist A vector of distances at which to return the variogram fit 
#'   values (defaults to 128 regularly-spaced values). 
#' 
#' @param ... Additional arguments (ignored)
#' 
#' @return A data.frame with the distances (column \code{dist}), the fitted 
#'   values (\code{gamma}), and if object contains more than one matrix, 
#'   a column \code{matrixn}. 
#' 
#' @examples 
#' 
#' \dontrun{ 
#' vario_indics <- variogram_sews(serengeti)
#' predict(vario_indics)
#' vario_test <- indictest(vario_indics, nulln = 19)
#' predict(vario_test) # same result
#' }
#' 
#' @seealso \code{\link{variogram_sews}}, 
#'   \code{\link[=variogram_sews_plot]{plot}}, 
#'   \code{\link[=variogram_sews_plot]{plot_variogram}}, 
#'   \code{\link{extract_variogram}}
#' 
#'@export 
predict.variogram_sews_list <- function(object, newdist = NULL, ...) { 
  all_preds <- llply(object, predict.variogram_sews_single, newdist = newdist)
  all_preds <- Map(function(n, o) { 
    data.frame(matrixn = n, o)
  }, seq_along(all_preds), all_preds)
  do.call(rbind, all_preds)
}
#'@export 
predict.variogram_sews_single <- function(object, newdist = NULL, ...) { 
  if ( is.null(newdist) ) { 
   newdist <- with(object[["variogram"]], seq(0, max(dist), length.out = 128))
  }
  pars <- object[["metrics"]]
  y <- spherical_model(newdist, pars[1], pars[2], pars[3])
  data.frame(dist = newdist, gamma = y)
}

# Plot methods for variogram sews 
#' 
#' @rdname variogram_sews_plot
#' @name variogram_sews_plot
#' 
#' @title Early-warning signals based on variograms 
#' 
#' @description Plot trends of indicators based on variograms
#' 
#' @param x An object produced by \code{\link{variogram_sews}}, or the 
#'   result of applying \code{indictest} on such object.
#' 
#' @param along A vector providing values along which the indicator trends 
#'   will be plotted. If \code{NULL} then the indicator values are plotted
#'   sequentially in their original order. 
#' 
#' @details The \code{plot()} function will display how the estimated 
#'   variogram parameters change along a set of values (passed with argument
#'   \code{along}). If the object passed has been processed through 
#'   \code{indictest}, then the null values are also displayed. 
#'   \code{plot_variogram()} can be used to display the individual variograms
#'   that have been fit to the data. 
#' 
#' @seealso 
#' 
#'   \code{\link{variogram_sews}}, \code{\link{indictest}}
#' 
#' @examples 
#'   
#' serengeti_ews <- variogram_sews(serengeti, subset_frac = 0.01, 
#'                                 model ="exp")
#' # Display the change in variogram parameters 
#' plot(serengeti_ews, along = serengeti.rain) + 
#'   ggplot2::labs(x = "Rainfall (mm)")
#' 
#' # Visualize the fitted variograms
#' plot_variogram(serengeti_ews, along = serengeti.rain) 
#' 
#' \dontrun{ 
#'   # Test the trends (nulln should be set to a higher value to obtain 
#'   # meaningful results
#'   serengeti_test <- indictest(serengeti_ews, nulln = 19)
#'   plot(serengeti_test, along = serengeti.rain)
#'   plot_variogram(serengeti_test, along = serengeti.rain)
#' }
#' 
#'@export
plot.variogram_sews <- function(x, along = NULL, ...) { 
  NextMethod("plot")
}
#'@export 
plot.variogram_sews_list <- function(x, along = NULL, ...) { 
  # Adjust x, then pass it on to the method for simple_sews plotting. Here 
  # we use a for loop so that the class attributes is preserved. 
  for (i in seq_along(x) ) { 
    x[["value"]] <- x[["metrics"]]
  }
  
  plot.simple_sews_list(x, along = along, ...)
}
#'@export 
plot.variogram_sews_single <- function(x, along = NULL, ...) { 
  stop(paste0('I cannot plot a trend with only one value ! Did you want to ', 
              'display a variogram? Use plot_variogram() instead. '))
}

#'@rdname variogram_sews_plot
#'@export
plot_variogram <- function(x, along = NULL, ...) { 
  UseMethod("plot_variogram")
}
#'@rdname variogram_sews_plot
#'@method plot_variogram variogram_sews
#'@export
plot_variogram.variogram_sews <- function(x, along = NULL, ...) { 
  UseMethod("plot_variogram")
}
#'@export
plot_variogram.variogram_sews_single <- function(x, along = NULL, ...) { 
  df <- extract_variogram(x)
  df_pred <- predict(x)
  
  ggobj <- ggplot(df, aes(x = dist, y = gamma)) + 
    geom_line(color = "red", data = df_pred) + 
    geom_point() + 
    geom_line() + 
    theme_spwarnings() + 
    linescale_spwarnings() + 
    labs(x = "Distance", 
         y = "gamma")
  
  return(ggobj)
}
#'@export
plot_variogram.variogram_sews_list <- function(x, along = NULL, ...) { 
  
  # Extract data and add along data
  df <- extract_variogram(x)
  df_pred <- predict(x)
  if ( is.null(along) ) { 
    df[ ,'along'] <- df[ ,"matrixn"]
    df_pred[ ,'along'] <- df_pred[ ,"matrixn"]
  } else { 
    df[ ,"along"] <- along[df[ ,"matrixn"]]
    df_pred[ ,"along"] <- along[df_pred[ ,"matrixn"]]
  }
  
  ggobj <- ggplot(df, aes_string(x = "dist", y = "gamma")) + 
    geom_line(color = "red", data = df_pred) + 
    geom_point() + 
    geom_line() + 
    theme_spwarnings() + 
    linescale_spwarnings() + 
    labs(x = "Distance", 
         y = "gamma") + 
    facet_wrap( ~ along, scales = "free_y")
  
  return(ggobj)
}


# Fit a variogram. We do it by hand and do not use gstat which seems to be 
# a bit unreliable. We initialize the varioram with a segmented regression 
# first, then refine that fit with bounded BFGS. 
fit_variogram <- function(mat, subset_frac, model, locations = NULL) { 
  # Check if mat is suitable
  check_mat(mat)
  
  # Sample cells from matrix
  if ( is.null(locations) ) { 
    if ( subset_frac == 1 ) { 
      locations <- cbind(seq.int(nrow(mat)), seq.int(ncol(mat)))
    } else { 
      cell_sample <- sample.int(length(mat), 
                                size = round(length(mat) * subset_frac), 
                                replace = FALSE)
      locations <- cbind(1 + (cell_sample - 1) %% nrow(mat), 
                         1 + (cell_sample - 1) %/% nrow(mat))
    }
  } 
  
  values <- FALSE
  tryn <- 1
  while ( tryn < 10 && length(unique(values)) < 2 ) { 
    values <- apply(locations, 1, function(X) mat[X[1], X[2]])
    tryn <- tryn + 1
  }
  
  # Format things to compute variogram with gstat
  locations.gstat <- sp::SpatialPointsDataFrame(locations, 
                                                data.frame(z = values))
  vario <- gstat::variogram(z ~ 1, 
                            data = locations.gstat, 
                            width = 5, 
                            cutoff = 100)
  
  # Check if sample has only a single type of values. In that case we return 
  # a default answer with missing values for parameters as the model will not 
  # fit.
  if ( length(unique(values)) < 2 ) { 
    default_answer <- 
      list(pars = list(nugget = NaN, 
                       psill  = NaN, 
                       range  = NaN), 
          vario = vario)
    return(default_answer)
  }
  
  # Fit variogram. We use weights as suggested by gstat::fit.variogram
  brk_model <- suppressWarnings(segmented::segmented(lm(gamma ~ dist, 
                                                        data = vario)))
  brk_coefs <- coef(brk_model)
  
  # Extract the break value, if there is one
  nugget0 <- max(0, as.vector(brk_coefs["(Intercept)"]))
  if ( !is.null(brk_model[["psi"]][2]) ) { 
    range0 <- brk_model[["psi"]][2]
    psill0 <- abs(nugget0 - median(vario[vario$dist > range0, "gamma"]))
  } else { 
    range0 <- 1/3 * (max(vario[ ,"dist"]) - min(vario[ ,"dist"]))
    psill0 <- 0
  }
  pars0 <- c(nugget = nugget0, psill  = psill0, range  = range0)
  
  # Objective function
  ssq <- function(pars0) { 
    if ( model == "sph" ) { 
      pred <- spherical_model(vario[ ,"dist"], pars0[1], pars0[2], pars0[3]) 
    } else if ( model == "exp" ) { 
      pred <- exponential_model(vario[ ,"dist"], pars0[1], pars0[2], pars0[3]) 
    }
    sum( ( (vario[ ,"gamma"] - pred) )^2 )
  }
  
  fit <- optim(pars0, ssq, method = "L-BFGS-B", 
               lower = c(0, 0, 0), 
               control = list(maxit = 20e3))

  vario[ ,"pred"] <- with(as.list(fit$par), 
                          spherical_model(vario[ ,"dist"], 
                                          nugget, psill, range))
#   with(vario, plot(dist, gamma))
#   with(vario, lines(dist, pred, col = "blue"))
#   with(vario, lines(dist, 
#                     spherical_model(dist, pars0[1], pars0[2], pars0[3]), 
#                     col = "red"))
  
  list(pars = as.list(fit$par), 
       vario = vario, 
       locations = locations)
}

spherical_model <- function(X, nugget, psill, range) { 
#   ifelse(X < range, 
#          nugget + psill * ( (3/2) * (X/range) - (1/2) * (X/range)^3 ), 
#          nugget + psill)
  # Exponential model 
  nugget + psill * (1 - exp(-X/range))
  
}

exponential_model <- function(X, nugget, psill, range) { 
  nugget + psill * (1 - exp(-X/range))
}

compute_vario_metrics <- function(pars) { 
  structvar <- with(pars, psill / (nugget + psill))
  metrics <- with(pars, c(nugget, psill, range, structvar))
  names(metrics) <- c('nugget', 'psill', 'range', 'structvar')
  return(metrics)
}

# Raw version of the function
#' 
#' @title Variogram parameters
#' 
#' @description Compute the nugget, partial sill, correlation range and 
#'   structural variance on a matrix. 
#' 
#' @param mat A matrix (TRUE/FALSE values) or a list of matrices 
#' 
#' @param subset_frac The number of points to consider in the matrix (as a 
#'   proportion of the total number of cells) used to computed the variogram. 
#'   A lower value here will speed up computations, at the expense of precision.
#' 
#' @param model The variogram model to use, either "sph" (for a spherical model)
#'   or "exp" (for an exponential model)
#' 
#'@export
raw_variogram_metrics <- function(mat, subset_frac = 0.5, model = "sph") { 
  if ( ! is.matrix(mat) ) { 
    stop("raw_variogram_metrics only accepts a single matrix as input.")
  }
  
  vario <- fit_variogram(mat, subset_frac = subset_frac, model = model)
  compute_vario_metrics(vario[["pars"]])
}

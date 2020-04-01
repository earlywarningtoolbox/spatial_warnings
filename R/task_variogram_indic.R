# 
# 
# 
variogram_sews <- function(mat, 
                           subset_frac = 0.10) { 
  require(gstat)
  require(sp)
  
  # Check if mat is suitable
  check_mat(mat)

  # Handle list case
  if ( is.list(mat) ) { 
    results <- lapply(mat, variogram_sews, 
                      subset_frac)
    names(results) <- names(mat)
    class(results) <- c('variogram_sews_list',  'variogram_sews', 
                        'sews_result_list', 'list')
    attr(results, 'indicname') <- "Variogram-based indicators"
    return(results)
  }
  
  # Returns a list with components 'vario' (the empirical variogram) and 
  # 'fit' (its fit)
  vario <- fit_variogram(mat, subset_frac)
  metrics <- compute_vario_metrics(vario[['fit']])
  
  # Return list containing both
  output <- list(variogram = vario[['vario']], 
                 fit = vario[['fit']], 
                 metrics = metrics, 
                 orig_data = mat, 
                 pars = list(subset_frac = subset_frac), 
                 call = match.call())
  class(output) <- c('variogram_sews_single', 'variogram_sews', 
                     'sews_result_single', 'list')
  attr(output, 'indicname') <- "Variogram-based indicators"
  
  return(output)
}



# Methods for regular function (without testing)
summary.variogram_sews_list <- function(x, ...) { 
  
  cat('Spatial Early-Warning:', attr(x, "indicname"), '\n') 
  cat('\n')
  display_size_info(x)
  cat('\n')
  
  summary.tab <- as.data.frame(x)
  names(summary.tab)[1] <- c('Mat. #')
  print.data.frame(summary.tab, row.names = FALSE, digits = DIGITS)
  
  cat('\n')
  cat('Use as.data.frame() to retrieve values in a convenient form\n')
  
  invisible(summary.tab)
}

summary.variogram_sews_single <- function(x, ...) { 
  x.list <- x
  attr(x.list, "indicname") <- attr(x, "indicname")
  summary.variogram_sews_list(x.list)
}


as.data.frame.variogram_sews_single <- function(x, ...) { 
  as.data.frame.variogram_sews_list(list(x))
}
as.data.frame.variogram_sews_list <- function(x, ...) { 
  # We display only summary statistics 
  values <- Map(function(n, o) { 
    data.frame(matrixn = n, as.data.frame(as.list(o[['metrics']])))
  }, seq_along(x), x)
  values <- do.call(rbind, values)
  values <- as.data.frame(values)
  return(values)
}


# Testing method 
indictest.variogram_sews_list <- function(x, 
                                          nulln = 999, 
                                          null_method = "perm", 
                                          ...) { 
  
  results <- parallel::mclapply(x, indictest.variogram_sews_single, 
                                nulln, null_method, ...)
  
  # Add matrixn column with correct matrixn number
  for ( nb in seq_along(results) ) { 
    results[[nb]][['matrixn']] <- nb
  }
  
  class(results) <- c('variogram_sews_test_list', 'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  return(results)
}

indictest.variogram_sews_single <- function(x, 
                                            nulln = 999, 
                                            null_method = "perm", 
                                            ...) { 
  
  metric_compute <- function(mat) { 
    vario <- fit_variogram(mat, subset_frac = x[['pars']][['subset_frac']])
    compute_vario_metrics(vario[['fit']])
  }
  
  test_values <- compute_indicator_with_null(x[['orig_data']], 
                                             nulln = nulln, 
                                             indicf = metric_compute, 
                                             null_method = null_method)
  # Format result
  results <- c(test_values, list(nulln = nulln))
  class(results) <- c('variogram_sews_test_single', 'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  return(results)
}

fit_variogram <- function(mat, subset_frac) { 
  
  # Sample cells from matrix
  rows <- sample(nrow(mat), size = round(nrow(mat)*subset_frac), 
                 replace = FALSE)
  cols <- sample(ncol(mat), size = round(ncol(mat)*subset_frac), 
                 replace = FALSE)
  values <- apply(cbind(rows, cols), 1, function(X) mat[X[1], X[2]])
  locations <- data.frame(x = rows, y = cols, z = values)
  coordinates(locations) <- ~ x + y
  
  # Compute variogram
  vario <- variogram(z ~ 1, 
                     data = locations)
  
  # Fit variogram 
  m <- vgm(psill = NA, nugget = TRUE, model = "Sph")
  # m <- vgm(model = "Sph")
  fit <- fit.variogram(vario, m, 
                       )
  
  list(fit = fit, 
       vario = vario)
}

compute_vario_metrics <- function(variofit) { 
    
  fitdf <- as.data.frame(variofit)
  nugget <- fitdf[1, "psill"] # ok...
  psill  <- fitdf[2, "psill"]
  range  <- fitdf[2, "range"]
  structvar <- psill / (nugget + psill)
  
  metrics <- c(nugget, psill, range, structvar)
  names(metrics) <- c('nugget', 'psill', 'range', 'structvar')
  return(metrics)
}


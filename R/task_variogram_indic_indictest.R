# 
# 

# 
# 
# 
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
  
  class(results) <- c('variogram_sews_test_list', 
                      'variogram_sews_list', 
                      'sews_test', 'list')
  attr(results, "indicname") <- attr(x, "indicname")
  return(results)
}

indictest.variogram_sews_single <- function(x, 
                                            nulln = 999, 
                                            null_method = "perm", 
                                            ...) { 
  
  # This function will produce a vector, with the 4 first values holding 
  # the summarized metrics, and the last ones the values of the variogram. The 
  # total number of values is the same, as the locations at which the variogram
  # is sampled stay constant. 
  metric_compute <- function(mat) { 
    a <- fit_variogram(mat, 
                       subset_frac = x[['pars']][['subset_frac']], 
                       model = x[["pars"]][["model"]], 
                       locations = x[["location"]]) 
    unlist(with(a, c(compute_vario_metrics(pars), vario[ ,"gamma"])))
  }
  
  test_values <- compute_indicator_with_null(x[['orig_data']], 
                                             nulln = nulln, 
                                             indicf = metric_compute, 
                                             null_method = null_method)
  
  # Format results. The first four values are parameters, the rest is the 
  # variogram.
  pars <- as.data.frame(do.call(cbind, 
                                lapply(test_values, function(o) o[1:4])))
  pars <- data.frame(indic = row.names(pars), pars)
  vario <- as.data.frame(do.call(cbind, 
                                 lapply(test_values, function(o) o[-(1:4)])))
  vario <- data.frame(x[["variogram"]], vario)
  row.names(vario) <- as.character(seq.int(nrow(vario)))
  
  # We replace or add things in x
  x[["variogram"]] <- vario
  x[["metrics_test"]] <- pars
  class(x) <- c('variogram_sews_test_single', 
                'variogram_sews_single', 
                'sews_test', 'list')
  return(x)
}

#'@export 
summary.variogram_sews_test_list <- function(object, ...) { 
  summary.simple_sews_test_list(object, ...)
}
#'@export 
summary.variogram_sews_test_single <- function(object, ...) { 
  summary.simple_sews_test_single(object, ...)
}

#'@export
print.variogram_sews_test_list <- function(x, ...) { 
  summary.variogram_sews_test_list(x, ...)
}
#'@export
print.variogram_sews_test_single <- function(x, ...) { 
  summary.variogram_sews_test_single(x, ...)
}

#'@export
as.data.frame.variogram_sews_test_list <- function(x, ...) { 
  all_df <- Map(function(n, o) { 
    data.frame(matrixn = n, o[["metrics_test"]])
  }, seq_along(x), x)
  do.call(rbind, all_df)
}
#'@export
as.data.frame.variogram_sews_test_single <- function(x, ...) { 
  data.frame(matrixn = 1, x[["metrics_test"]])
}

#'@export
plot.variogram_sews_test_list <- function(x, along = NULL, 
                                          what = "value", 
                                          display_null = TRUE, ...) { 
  plot.simple_sews_test_list(x, along, what, display_null, ...)
}
#'@export
plot.variogram_sews_test_single <- function(x, along = NULL, 
                                            what = "value", 
                                            display_null = TRUE, ...) { 
  plot.simple_sews_test_single(x, along, what, display_null, ...)
}

#'@export
plot_variogram.variogram_sews_test_list <- function(x, along = NULL, ...) { 
  ggobj <- plot_variogram.variogram_sews_list(x, along = along)
  
  # Extract null values and display them 
  variodf <- extract_variogram(x)
  if ( ! is.null(along) ) { 
    variodf[ ,"along"] <- along[variodf[ ,"matrixn"]]
  } else { 
    variodf[ ,"along"] <- variodf[ ,"matrixn"]
  }
  
  ggobj$layers <- c(geom_line(aes_string(x = "dist", y = "null_mean"), 
                              data = variodf, 
                              color = 'black', alpha = .1), 
                    geom_ribbon(aes_string(x = "dist", ymin = "null_05", 
                                           ymax = "null_95"), 
                                data = variodf, 
                                fill = 'grey',
                                group = 1, 
                                alpha = .8), 
                    ggobj$layers)
  return(ggobj)
}
#'@export
plot_variogram.variogram_sews_test_single <- function(x, ...) { 
  browser()
  ggobj <- plot_variogram.variogram_sews_single(x)
  # Extract null values and display them 
  variodf <- extract_variogram(x)
  
  ggobj$layers <- c(geom_line(aes_string(x = "dist", y = "null_mean"), 
                              data = variodf, 
                              color = 'black', alpha = .1), 
                    geom_ribbon(aes_string(x = "dist", ymin = "null_05", 
                                           ymax = "null_95"), 
                                data = variodf, 
                                fill = 'grey',
                                group = 1, 
                                alpha = .8), 
                    ggobj$layers)
  return(ggobj)
}

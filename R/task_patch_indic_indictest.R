# # 
# # Functions that help compute significance of patch-size distributions
# # 
# #' @rdname patchdistr_sews
# #' 
# #' @param x An object returned by \code{patchdistr_sews}
# #' 
# #' @param nulln The number of simulations to use to compute the null 
# #' distribution of indicator values 
# #' 
# #' @param null_method The method to use to compute null indicator values 
# #'   (can be one of "perm", "bernouilli", or a function, see Details)
# #' 
# #' @param ... Additional arguments are ignored. 
# #'
# #'@export
# indictest.patchdistr_sews <- function(x, 
#                                       nulln = 999, 
#                                       null_method = "perm", 
#                                       ...) { 
#   NextMethod("indictest")
# }

#'@export
indictest.patchdistr_sews_single <- function(x, 
                                             nulln = 999, 
                                             null_method = "perm", 
                                             ...) { 
  
  
  # Obtain null distributions + plrange values. We compute both at the same 
  # time so we do not reshuffle matrices twice. 
  nulldistr <- generate_null_distr(x[["orig_data"]], 
                                   nulln = nulln, 
                                   null_method = null_method, 
                                   function(nullmat) { 
      list(plrange = raw_plrange(nullmat), 
           psd = patchsizes(nullmat))
  })
  
  # Extract plrange from the above computation all the unique patch sizes, 
  # then compute P-values, etc. 
  # We only report a p-value if the observed plrange is all right. 
  plrange_null_distr <- unlist(lapply(nulldistr, function(o) o[["plrange"]]))
  if ( is.na(x[["plrange"]][["plrange"]]) ) { 
    plrange_pval <- NA_real_
  } else { 
    # Compute the actual P-value
    plrange_pval <- rank(c(x[["plrange"]][["plrange"]], plrange_null_distr))[1] / 
                      (1 + nulln)
    plrange_pval <- 1 - plrange_pval
  }
  
  # Get a vector of x values at which to evalue the null distribution 
  max_null_patch_size <- unlist(lapply(nulldistr, function(o) max(o[["psd"]])))
  max_null_patch_size <- max(max_null_patch_size)
  xpsd <- exp( seq(log(1), to = log(max_null_patch_size), 
                   length.out = 512))
  xpsd <- unique(round(xpsd))
  
  # Extract null distributions and get quantiles of the inverse cumulative 
  # null distribution. 
  cumpsd_null_distr <- lapply(nulldistr, function(o) { 
    cumpsd(o[["psd"]], x = xpsd)
  })
  cumpsd_null_distr <- do.call(rbind, cumpsd_null_distr)
  cumpsd_null_distr <- ddply(cumpsd_null_distr, ~ patchsize, function(df) { 
    data.frame(q05 = quantile(df[ ,"y"], 0.05), 
               q95 = quantile(df[ ,"y"], 0.95), 
               median = median(df[ ,"y"]), 
               mean   = mean(df[ ,"y"]))
  })
#   ggplot(cumpsd_null_distr, aes(x = patchsize)) + 
#     geom_ribbon(aes(ymin = q05, ymax = q95), 
#                 alpha = .2) + 
#     geom_line(aes(y = median), linetype = "dashed", color = "red") + 
#     geom_point(aes(y = y), data = cumpsd(x$psd_obs)) + 
#     geom_line(aes(y = y), data = cumpsd(x$psd_obs)) + 
#     scale_x_log10() + 
#     scale_y_log10() 
  ans <- c(x, 
           cumpsd_null = list(cumpsd_null_distr), 
           plrange_pval = plrange_pval, 
           nulln = nulln, 
           null_method = null_method)
  class(ans) <- c('patchdistr_sews_test_single', 
                  'patchdistr_sews_single', 
                  'sews_result_single', 
                  'sews_test', 
                  'list')
  return(ans)
}

#'@export
indictest.patchdistr_sews_list <- function(x, 
                                           nulln = 999, 
                                           null_method = "perm", 
                                           ...) { 
  
  # Compute a distribution of null values for SDR
  results <- future.apply::future_lapply(x, indictest.patchdistr_sews_single, 
                                         nulln, null_method)
  
  # Transfer names 
  names(results) <- names(x)
  
  # Format and return output
  class(results) <- c('patchdistr_sews_test_list', 
                      'patchdistr_sews_list', 
                      'sews_result_list', 
                      'sews_test', 'list')
  
  return(results)
  
}


# Print/Summary/etc. methods, which actually reuse most of the code from 
# the non-indictest version
#'@export
print.patchdistr_sews_test_list <- function(x, ...) { 
  summary.patchdistr_sews_test_list(x, ...)
}

#'@export
print.patchdistr_sews_test_single <- function(x, ...) { 
  summary.patchdistr_sews_test_single(x, ...)
}

#'@export
summary.patchdistr_sews_test_list <- function(object, ...) { 
  summary.patchdistr_sews(object, ...)
}

#'@export
summary.patchdistr_sews_test_single <- function(object, ...) { 
  summary.patchdistr_sews(object, ...)
}

# Convert each element to a data frame, and a column with the matrixn number
#'@export
as.data.frame.patchdistr_sews_test_list <- function(x, ...) { 
  newdat <- llply(x, as.data.frame.patchdistr_sews_test_single)
  newdat <- Map(function(n, o) data.frame(matrixn = n, o), 
                seq_along(newdat), newdat)
  do.call(rbind, newdat)
}
#'@export
as.data.frame.patchdistr_sews_test_single <- function(x, ...) { 
  # Do the same as non-test version, but add the pvalue for the plrange
  dat <- as.data.frame.patchdistr_sews_single(x)
  data.frame(dat, plrpval = x[["plrange_pval"]])
}

#'@export
plot_distr.patchdistr_sews_test_list <- function(x, 
                                                 along = NULL, 
                                                 best_only = TRUE, 
                                                 plrange = TRUE) { 
  
  gplot <- plot_distr.patchdistr_sews_list(x, along, best_only, plrange)
  
  # Add the null data to that plot
  nulldat <- Map(function(n, o) data.frame(matrixn = n, o[['cumpsd_null']]), 
                 seq_along(x), x)
  nulldat <- do.call(rbind, nulldat)
  if ( ! is.null(along) ) { 
    nulldat[ ,'matrixn'] <- along[nulldat[ ,'matrixn']]
  }
  
  # NOTE: we add layers this way to the ggplot object, so null values appear 
  # below the observed ones
  gplot$layers <- c(geom_ribbon(aes_q(x = ~patchsize, 
                                      ymin = ~q05, 
                                      ymax = ~q95), 
                                data = nulldat, alpha = .2), 
                    gplot$layers)
  
  return(gplot)
}

#'@export
plot_distr.patchdistr_sews_test_single <- function(x, 
                                                   along = NULL, 
                                                   best_only = TRUE, 
                                                   plrange = TRUE) { 
  
  gplot <- plot_distr.patchdistr_sews_single(x, along, best_only, plrange)
  
  # Add the null data to that plot. We modify the layers so that the ribbon 
  # appears underneath the observed values
  gplot$layers <- c(geom_ribbon(aes_q(x = ~patchsize, 
                                      ymin = ~q05, 
                                      ymax = ~q95), 
                                data = x[['cumpsd_null']], alpha = .2), 
                    gplot$layers)

  return(gplot)
}


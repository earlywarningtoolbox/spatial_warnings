
#'@export
indictest.patchdistr_sews_single <- function(x, 
                                             nulln = 999, 
                                             null_method = 'perm', 
                                             null_control = NULL, 
                                             ...) { 
  
  null_control <- null_control_set_args(x, null_control)
  
  # Obtain null distributions + plrange values. We compute both at the same 
  # time so we do not reshuffle matrices twice. 
  null_results <- generate_nulls(x[["orig_data"]], 
                                 nulln = nulln, 
                                 null_method = null_method, 
                                 null_control = null_control, 
                                 function(nullmat) { 
      list(plrange = raw_plrange(nullmat), 
           psd = patchsizes(nullmat))
  })
  
  # Extract plrange from the above computation all the unique patch sizes, 
  # then compute P-values, etc. 
  # We only report a p-value if the observed plrange is all right. 
  plrange_null_distr <- laply(null_results[["nulldistr"]], 
                              function(o) o[["plrange"]])
  
  if ( is.na(x[["plrange"]][["plrange"]]) ) { 
    plrange_pval <- NA_real_
  } else { 
    # Compute the actual P-value
    plrange_pval <- 
      rank(c(x[["plrange"]][["plrange"]], plrange_null_distr))[1] / (1 + nulln)
    plrange_pval <- 1 - plrange_pval
  }
  
  # Get a vector of x values at which to evalue the null distribution 
  max_null_patch_size <- laply(null_results[["nulldistr"]], 
                               function(o) max(o[["psd"]]))
  max_null_patch_size <- max(max_null_patch_size)
  xpsd <- exp( seq(log(1), to = log(max_null_patch_size), 
                   length.out = 512))
  xpsd <- unique(round(xpsd))
  
  # Extract null distributions and get quantiles of the inverse cumulative 
  # null distribution. 
  cumpsd_null_distr <- ldply(null_results[["nulldistr"]], function(o) { 
    cumpsd(o[["psd"]], x = xpsd)
  })
  cumpsd_null_distr <- ddply(cumpsd_null_distr, ~ patchsize, function(df) { 
    data.frame(qinf = quantile(df[ ,"y"], null_control[["qinf"]]), 
               qsup = quantile(df[ ,"y"], null_control[["qsup"]]), 
               median = median(df[ ,"y"]), 
               mean   = mean(df[ ,"y"]))
  })
  
  ans <- c(x, 
           cumpsd_null = list(cumpsd_null_distr), 
           plrange_pval = plrange_pval, 
           nulln = nulln, 
           null_method = null_method, 
           null_control = null_control)
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
                                           null_method = 'perm', 
                                           null_control = NULL, 
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
                                      ymin = ~qinf, 
                                      ymax = ~qsup), 
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
                                      ymin = ~qinf, 
                                      ymax = ~qsup), 
                                data = x[['cumpsd_null']], alpha = .2), 
                    gplot$layers)

  return(gplot)
}


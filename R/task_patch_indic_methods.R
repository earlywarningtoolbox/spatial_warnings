# 
# 
# This file contains methods related to objects of the patchdistr task
# 


#'@export
as.data.frame.patchdistr_spews_single <- function(obj) { 
  data.frame(replicate   = 1, 
             model       = names(obj[['models']]),
             likelihoods = unlist(obj[['likelihoods']]), 
             is_best     = names(obj[['models']]) == obj[['best']])
}

#'@export
as.data.frame.patchdistr_spews_list <- function(obj) { 
  # Format data
  results <- lapply(obj, as.data.frame.patchdistr_spews_single)
  results <- Map(function(n, df) { df[ ,'replicate'] <- n; df }, 
                 seq.int(length(results)), results)
  
  # Bind it altogether and return
  do.call(rbind, results)
}





#'@export
plot.patchdistr_spews_list <- function(obj) { 
  browser()
  
  obj_table <- as.data.frame(obj)
    
  
}




#'@export
plot_distr <- function(obj, all.models = TRUE) { 
  UseMethod('plot_distr')
}

#'@export
plot_distr.patchdistr_spews_single <- function(obj, all.models = TRUE) { 
  
  # Get plottable data.frames
  values <- predict(obj, all.models = all.models)
  
  # Check if there are plottable things
  if ( is.na(values[['obs']][ ,'x']) ) { 
    stop('No patch size distribution fitted')
  }
  
  ggplot() + 
    geom_point(aes(x = x, y = y), 
               data = values[['obs']]) + 
    geom_line(aes(x = x, y = y, color = model), 
              data = values[['pred']]) + 
    scale_y_log10() +
    scale_x_log10()
}

#'@export
plot_distr.patchdistr_spews_list <- function(obj, all.models = TRUE) { 
  
  # Get plottable data.frames
  values <- predict(obj, all.models = all.models)
  
  # Remove values where no data is available
  na_distribs <- values[['pred']][is.na(values[['pred']][ ,'x']), 'replicate']
  na_distribs <- unique(na_distribs) 
  if ( length(na_distribs) > 0 ) { 
    values <- lapply(values, function(x) { 
                       x[ !x[ ,'replicate'] %in% na_distribs, ]
                     })
    warning('Removed ', length(na_distribs), ' replicate', 
            ifelse(length(na_distribs)>0, 's', ''), 
            ' due to missing values', sep = '')
  } 
  
  ggplot() + 
    geom_point(aes(x = x, y = y), 
               data = values[['obs']]) + 
    geom_line(aes(x = x, y = y, color = model), 
              data = values[['pred']]) + 
    scale_y_log10() +
    scale_x_log10() + 
    facet_wrap( ~ replicate) + 
    xlab('Patch size') + 
    ylab('Frequency (P>x)')
    
}




# Predict methods (retrieve data)
#'@export
predict.patchdistr_spews_single <- function(obj, all.models = TRUE) { 
  
  
  if ( obj[['best']] == 'none' ) { 
    warning('Cannot predict values with no model fit: returning NA')
    na_dat <- data.frame(x = NA, y = NA)
    return( list(obs = na_dat, pred = na_dat) )
  }
  
  # Get observed values
  vals_obs <- plot(obj[['models']][[1]], draw = FALSE)
  
  # Predict values
  vals_pred <- Map(function(n, dat) { 
                          data.frame(model = n, 
                                     get_predicted_vals(dat, vals_obs)) 
                        }, 
                        names(obj[['models']]), obj[["models"]])
  vals_pred <- do.call(rbind, vals_pred)
  
  # Cut predicted values within range of what is observed
  lower_bound <- min(vals_obs[ ,'y']) 
  vals_pred <- vals_pred[vals_pred[ ,'y'] > lower_bound, ]
  
  # Honor the user choice to be only interested in the best model 
  if ( ! all.models ) { 
    best <- obj[['best']]
    vals_pred <- vals_pred[vals_pred[ ,'model'] == best, ]
  }
  
  # Return data
  return( list(obs = vals_obs, pred = vals_pred) )
}

#'@export
predict.patchdistr_spews_list <- function(obj, all.models = TRUE) { 
  
  dat <- lapply(obj, predict.patchdistr_spews_single, all.models)
  
  dat <- Map(function(n, x) { 
               x[['obs']]  <- data.frame(replicate = n, x[['obs']])
               x[['pred']] <- data.frame(replicate = n, x[['pred']])
               x
             }, seq.int(length(dat)), dat)
  
  # Convert to data.frame
  dat <- list(obs  = plyr::ldply(dat, function(x) x[['obs']]), 
              pred = plyr::ldply(dat, function(x) x[['pred']]))
  
  return(dat)
}




# This helper functions calls the right method in case the fit is 
#   poweRlaw-based or pli-based
get_predicted_vals <- function(obj, data) { 
  
  get_vals_if_tpl <- function(obj) { 
    xvals <- round(seq(1, max(data), length.out = 100))
    yvals <- ppowerexp(xvals, threshold = 1, exponent = obj[['exponent']], 
                       rate = obj[['rate']], lower.tail = FALSE)
    data.frame(x = xvals, y = yvals)
  }
  
  # If the fit is poweRlaw-based then we rely on its lines() method, otherwise
  #   we use ppowerexp from pli
  result <- wrap(obj, 
                 function(x) { lines(x, length.out = 100, draw = FALSE) }, # use lines() methods in poweRlaw package
                 get_vals_if_tpl) # use adequate function from pli
  
  # We do an explicit return so the return is not invisible (easier debug)
  return(result) 
}

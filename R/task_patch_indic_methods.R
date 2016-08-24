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
plot_distr <- function(obj, best_only = FALSE) { 
  UseMethod('plot_distr')
}

#'@export
plot_distr.patchdistr_spews_single <- function(obj, best_only = FALSE) { 
  
  # Get plottable data.frames
  values <- predict(obj, best_only = best_only)
    
  ggplot() + 
    geom_point(aes(x = patchsize, y = y), data = values[['obs']]) + 
    geom_line(aes(x = patchsize, y = y, color = type), 
              data = values[['pred']]) + 
    scale_y_log10() +
    scale_x_log10()
}

#'@export
plot_distr.patchdistr_spews_list <- function(obj, best_only = FALSE) { 
  
  # Get plottable data.frames
  values <- predict(obj, best_only = best_only)
  
  ggplot() + 
    geom_point(aes(x = patchsize, y = y), 
               data = values[['obs']]) + 
    geom_line(aes(x = patchsize, y = y, color = type), 
              data = values[['pred']]) + 
    scale_y_log10() +
    scale_x_log10() + 
    facet_wrap( ~ replicate) + 
    xlab('Patch size') + 
    ylab('Frequency (P>x)')
    
}




# Predict methods (retrieve data)
#'@export
predict.patchdistr_spews_single <- function(obj, 
                                            newdata = NULL,
                                            best_only = FALSE) { 
  
  
  # Get observed values
#   vals_obs <- plot(obj[['models']][[1]], draw = FALSE)
  vals_obs <- cumpsd(obj[["psd_obs"]])
  
  # Create x vector of values
  if ( is.null(newdata) ) { 
    newdata <- unique( round( seq(min(obj[["psd_obs"]]), 
                                  max(obj[["psd_obs"]]), 
                                  length.out = 1000) ) )
  }
  
  # Shapes table
  shptbl <- obj[['psd_shapes']]
  
  if ( best_only ) { 
    shptbl <- shptbl[shptbl[ ,'best'], ]
  }
  
  # Get values
  vals_pred <- data.frame()
  for ( type in shptbl[ ,"type"] ) { 
    type_yvals <- switch(type, 
                         pl  = ppl(newdata, shptbl[type, "expo"]),
                         tpl = ptpl(newdata, shptbl[type, "expo"], 
                                    shptbl[type, "rate"]),
                         exp = pdisexp(newdata, shptbl[type, "rate"]),
                         lnorm = pdislnorm(newdata, 
                                           shptbl[type, "meanlog"], 
                                           shptbl[type, "sdlog"])) 
    vals_pred <- rbind(vals_pred, 
                       data.frame(type = type, patchsize = newdata, y = type_yvals))
  }
  
  # Crop data to y range
  vals_pred <- vals_pred[ vals_pred[ ,'y'] >= min(vals_obs[ ,'y']) & 
                            vals_pred[ ,'y'] <= max(vals_obs[ ,'y']), ] 
  
  # Return data
  return( list(obs = vals_obs, 
               pred = vals_pred) )
}

#'@export
predict.patchdistr_spews_list <- function(obj, newdata = NULL, best_only = FALSE) { 
  
  dat <- lapply(obj, predict.patchdistr_spews_single, newdata, best_only)
  
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

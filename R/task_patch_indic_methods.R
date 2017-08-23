# 
# 
# This file contains methods related to objects of the patchdistr task
# 



# Plot methods
# --------------------------------------------------
# 
#' @rdname patchdistr_spews
#' 
#' @param along A vector providing values over which the indicator trend 
#'   will be plotted. If \code{NULL} then the values are plotted sequentially 
#'   in their original order. 
#' 
#' @param ... Further arguments passed to methods
#' 
#'@method plot patchdistr_spews
#'@export
plot.patchdistr_spews <- function(x, along = NULL, ...) { 
  if ( 'patchdistr_spews_single' %in% class(x) ) { 
    stop('I cannot plot a trend with only one value')
  }
  
  plot.patchdistr_spews_list(x, along)
}

# Note: plot will display how characteristics change along a gradient. To 
#   have a plot of distributions, use plot_distr
# 
#'@method plot patchdistr_spews_list
plot.patchdistr_spews_list <- function(x, along = NULL) { 
  
  if ( !is.null(along) && (length(along) != length(x)) ) { 
    stop('The along values are unfit for plotting (size mismatch)')
  }
  
  obj_table <- as.data.frame(x)
  
  # Subset table for best fits
  obj_table <- obj_table[is.na(obj_table[ ,'best']) | obj_table[ ,'best'], ]
  
  # If along is provided, then add it to the table
  xtitle <- deparse(substitute(along))
  if ( is.null(along) ) { 
    along <- as.factor(obj_table[ ,'replicate'])
    xtitle <- "Matrix number"
  }
  
  obj_table[ ,"along"] <- along
  
  # Now we summarise the obj_table
  alltypes <- na.omit(unique(obj_table[ ,"type"]))
  
  summ <- ddply(obj_table, 'along',
                function(df) { 
                  type_freqs <- sapply(alltypes, 
                         function(current_type) { 
                           sum(!is.na(df[ ,'type']) & df[ 'type'] == current_type)
                         })
                  type_freqs <- type_freqs / ifelse(sum(type_freqs) > 0, 
                                                    sum(type_freqs), 1)

                  data.frame(type = alltypes, type_freq = type_freqs, 
                             percolation = mean(df[ ,'percolation']), 
                             percolation_empty = mean(df[ ,'percolation_empty']), 
                             cover = mean(df[ ,"cover"]), 
                             # We remove NAs as sometime the power-law range cannot 
                             # be computed (not enough points)
                             plrange = mean(df[ ,'plrange'], na.rm = TRUE))
                  })
  
  # Make a data.frame with dummy facets 
  classif_df <- data.frame(plot_type = "PSD Classif.", summ)
  classif_df[ ,'plrange'] <- NA
  
  plrange_df <- ddply(summ, "along", function(df) { 
                       data.frame(plot_type = "PL-range", 
                                  plrange = df[1, "plrange"])
                      })
  
  summ <- rbind.fill(classif_df, plrange_df)
  
  # Construct base plot
  plot <- ggplot(summ) + 
    theme_spwarnings() + 
    fillscale_spwarnings(name = "PSD type") + 
    scale_linetype_discrete(name = "Cover") + 
    facet_grid( plot_type ~ ., switch = "y") + 
    ylab('') + 
    xlab(xtitle)
  
  if ( ! is.numeric(along) ) { 
    # Note that it is quite tricky to make ggplot produce a line over factors: 
    # `group=1` seems to do the trick. 
    plot <- plot + 
      geom_bar(aes_q(x = ~along, y = ~type_freq, fill = ~type), 
               position = "stack", stat = "identity") + 
      stat_summary(aes_q(x = ~along, y = ~percolation, 
                         linetype = "Perc. (full)", group = 1), 
                   fun.y = mean, geom = "line") + 
      stat_summary(aes_q(x = ~along, y = ~percolation_empty, 
                         linetype = "Perc. (empty)", group = 1), 
                   fun.y = mean, geom = "line") + 
      stat_summary(aes_q(x = ~along, y = ~cover, 
                         linetype = "Mean cover", group = 1), 
                   fun.y = mean, geom = "line") 
      
  } else { 
    plot <- plot + 
      geom_area(aes_q(x = ~along, y = ~type_freq, fill = ~type), 
                position = "stack") + 
      geom_line(aes_q(x = ~along, y = ~percolation, linetype = "Perc. (full)"))  + 
      geom_line(aes_q(x = ~along, y = ~percolation_empty, linetype = "Perc. (empty)"), 
                color = 'black') +
      geom_line(aes_q(x = ~along, y = ~cover, linetype = "Mean cover"), 
                color = 'black') 
      
  }
  
  # Add plrange to the graph
  plot <- plot + 
      geom_point(aes_q(x = ~along, y = ~plrange, group = 1), 
                stat = "identity") + 
      geom_line(aes_q(x = ~along, y = ~plrange, group = 1), 
                stat = "identity") 
  
  return(plot)
}



#' @rdname patchdistr_spews
#' 
# // along arg is already documented in plot() method
#' 
#' @param best_only Plot the empirical (inverse cumulative) patch-size 
#' distribution with an overlay of the estimated fits. 
#' 
#' @param plrange Plot the power-law range 
#'
#'@export
plot_distr <- function(x, along = NULL, best_only = TRUE, plrange = TRUE) { 
  UseMethod('plot_distr')
}

#'@export
plot_distr.patchdistr_spews_single <- function(x, 
                                               along = NULL, 
                                               best_only = TRUE, 
                                               plrange = TRUE) { 
  
  if ( !is.null(along) ) { 
    warning('Ignoring along parameter when plotting only one patch-size', 
            'distribution')
  }
  
  # Get plottable data.frames
  values <- predict(x, best_only = best_only)  
  
  # Create base plot 
  plot <- ggplot() + 
    scale_y_log10() +
    scale_x_log10() + 
    xlab('Patch size') + 
    ylab('Frequency (P>=x)') + 
    theme_spwarnings()
  
  # If plrange exists, then add it to the plot 
  plrange_dat <- x[['plrange']]
  if ( plrange && !is.na(plrange_dat[ ,"plrange"]) ) { 
    plrange_dat[ ,"xmax"] <- max(values[["obs"]][ ,"patchsize"])
    plot <- plot + 
      geom_segment(aes_q(x = ~xmin_est, y = 1, 
                         xend = ~xmax,  yend = 1), 
                   data = plrange_dat, 
                   color = "blue")
  }
  
  # Add observed values
  plot <- plot + 
    geom_point(aes_string(x = "patchsize", y = "y"), data = values[['obs']]) 
  
  # It can happen that no distribution have been fitted. Check for that 
  # before plotting otherwize it produces an error. 
  if ( any(!is.na(values[['pred']][ ,"y"])) ) { 
    plot <- plot + 
      geom_line(aes_string(x = "patchsize", y = "y", color = "type"), 
                data = values[["pred"]])
      
  } else { 
    warning('No distribution has been fitted to the observed patch size distribution')
  }
  
  return(plot)
}

#'@export
plot_distr.patchdistr_spews_list <- function(x, 
                                             along = NULL, 
                                             best_only = TRUE, 
                                             plrange = TRUE) { 
  
  if ( !is.null(along) && (length(along) != length(x)) ) { 
    stop('The along values are unfit for plotting (size mismatch)')
  }
  
  # Get plottable data.frames
  values <- predict(x, best_only = best_only)
  # Modify replicate column if necessary and reorder values
  if ( ! is.null(along) ) { 
    values[['obs']][ ,'replicate']  <- along[values[["obs"]][ ,'replicate']]
    values[['pred']][ ,'replicate'] <- along[values[["pred"]][ ,'replicate']]
  }
  
  plot <- ggplot() + 
    scale_y_log10() +
    scale_x_log10() + 
    facet_wrap( ~ replicate) + 
    xlab('Patch size') + 
    ylab('Frequency (P>=x)') + 
    theme_spwarnings()
  
  if ( plrange ) { 
    # Add plrange to the plot. We need to extract info 
    # from the observed psd so that we can place the segment on the plot. 
    plrange_dat <- unique( as.data.frame(x)[ ,c("replicate", 'xmin_est')] )
    if ( ! is.null(along) ) { 
      plrange_dat[ ,"replicate"] <- along[plrange_dat[ ,"replicate"]]
    }
    
    patches_minmax <- ddply(values[['obs']], "replicate", 
                          function(df) { 
                            data.frame(xmax = max(df[ ,"patchsize"]))
                          })
    plrange_dat <- join(plrange_dat, patches_minmax, type = "left", 
                        match = "first", by = "replicate")
    
    plot <- plot + 
      geom_segment(aes_q(x = ~xmin_est, y = 1, 
                          xend = ~xmax,  yend = 1), 
                    data = plrange_dat, 
                    color = "blue") 
  
  }
  
  # Add observed values
  plot <- plot + 
    geom_point(aes_string(x = "patchsize", y = "y"), 
               data = values[['obs']]) 
  
  # It can happen that no distribution have been fitted. Check for that 
  # before plotting otherwize it produces an error. 
  if ( any(!is.na(values[['pred']][ ,"y"])) ) { 
    # Get data and remove NAs (where no fit has been carried out
    pred_values <- values[["pred"]]
    pred_values <- pred_values[!is.na(pred_values[ ,"type"]), ]
    
    plot <- plot + 
              geom_line(aes_string(x = "patchsize", y = "y", color = "type"), 
                        data = pred_values)

  } else { 
    warning('No distribution has been fitted to the observed patch size distributions')
  }
  
  return(plot)
}





# Predict methods 
# --------------------------------------------------

#'@export
predict.patchdistr_spews_single <- function(object, ..., 
                                            newdata = NULL,
                                            best_only = FALSE) { 
  
  # Get observed values
  vals_obs <- cumpsd(object[["psd_obs"]])
  
  # Shapes table
  shptbl <- object[['psd_type']]
  
  # Bail if no fit carried out. Note that we need to set classes in the 
  # output pred df otherwise coercion when merging with existing results 
  # goes wrong (e.g. factor converted to integer).
  if ( all(is.na(shptbl[ ,'type'])) ) { 
    result <- list(obs = vals_obs, 
                   pred = data.frame(type = NA_character_, 
                                     patchsize = NA_integer_, 
                                     y = NA_real_))
    return(result)
  }
  
  # Create x vector of values
  if ( is.null(newdata) ) { 
    newdata <- unique( round(10^(seq(0, log10(max(object[["psd_obs"]])), 
                                     length.out = 200))) )
  }
  
  if ( best_only ) { 
    shptbl <- shptbl[shptbl[ ,'best'], ]
  }
  
  # Get values
  vals_pred <- data.frame()
  for ( type in shptbl[ ,"type"] ) { 
    type_yvals <- switch(type, 
                         pl  = ppl(newdata, 
                                   shptbl[type, "expo"], 
                                   shptbl[type, "xmin_fit"]),
                         tpl = ptpl(newdata, 
                                    shptbl[type, "expo"], 
                                    shptbl[type, "rate"], 
                                    shptbl[type, 'xmin_fit']),
                         exp = pdisexp(newdata,  
                                       shptbl[type, "rate"], 
                                       shptbl[type, "xmin_fit"]),
                         lnorm = pdislnorm(newdata, 
                                           shptbl[type, "meanlog"], 
                                           shptbl[type, "sdlog"],  
                                           shptbl[type, "xmin_fit"]))
    
    vals_pred <- rbind(vals_pred, 
                       data.frame(type = type, patchsize = newdata, 
                                  y = type_yvals))
  }
  
  # Crop data to y range
  vals_pred <- vals_pred[ vals_pred[ ,'y'] >= min(vals_obs[ ,'y']), ] 
  
  # Return data
  return( list(obs = vals_obs, 
               pred = vals_pred) )
}

#'@export
predict.patchdistr_spews_list <- function(object, ..., 
                                          newdata = NULL, best_only = FALSE) { 
  
  dat <- lapply(object, predict.patchdistr_spews_single, 
                newdata = newdata, best_only = best_only)
  
  # Add id but handle when psd is empty
  add_id <- function(n, x) { 
    if (nrow(x) > 0) { 
      x <- data.frame(replicate = n, x) 
    } else {
      x <- data.frame(replicate = n, patchsize = NA_integer_)
    } 
  }
  
  dat <- Map(function(n, x) { 
               x[['obs']]  <- add_id(n, x[["obs"]])
               x[['pred']] <- add_id(n, x[["pred"]])
               x
             }, seq.int(length(dat)), dat)
  
  # Convert to data.frame
  dat <- list(obs  = plyr::ldply(dat, function(x) x[['obs']]), 
              pred = plyr::ldply(dat, function(x) x[['pred']]))
  
  return(dat)
}




# As data.frame methods
# --------------------------------------------------

#'@export
as.data.frame.patchdistr_spews_single <- function(x, ...) { 
  ans <- data.frame(x[['psd_type']], x[['plrange']],
                    # Add the name explicitely for these as they are single values
                    percolation = x[['percolation']], 
                    percolation_empty = x[['percolation_empty']], 
                    cover = x[['cover']], 
                    npatches = x[['npatches']],
                    unique_patches = x[['unique_patches']], 
                    stringsAsFactors = FALSE)
  # We convert the psd type to a character vector for simplicity
  ans[ ,'type'] <- as.character(ans[ ,'type'])
  ans
}

#'@export
as.data.frame.patchdistr_spews_list <- function(x, ...) { 
  
  # Format data
  results <- lapply(x, as.data.frame.patchdistr_spews_single)
  results <- Map(function(n, df) { 
                   data.frame(replicate = n, df) 
                 }, seq.int(length(results)), results)
  
  # Bind it altogether and return df 
  do.call(rbind, results)
}




# Summary methods
# --------------------------------------------------

prepare_summary_table <- function(x, ...) { 
  
  dat <- as.data.frame(x)
  # Select lines that either have no fit or select the best fit
  dat <- dat[is.na(dat[ ,"best"]) | dat[ ,"best"], ]
  
  # Add formated unique patch column 
  dat[ ,'pretty_patches'] <- with(dat, 
                                  paste0(npatches, "(", unique_patches, ")"))
  # Add formated plrange column
  dat[ ,'plrange'] <- with(dat, 
                           ifelse(is.na(plrange), "", 
                                  paste0(round(plrange * 100), "%")))
  
  # Add formated distribution type column
  dat[ ,'type'] <- ifelse(is.na(dat[ ,'type']), " ", dat[ ,'type'])
  
  # Set the column names to extract
  cols <- c('pretty_patches', 'cover', 'percolation', 'percolation_empty',
            'type', 'plrange')
  pretty_names <- c('N(uniq.)', 'Cover', 'Percl.Full', 'Percl.Empt', 'Type', 'PLR')
  
  # If there is a replicate column (for when several matrices were 
  #   used at once), then add it to these columns
  if ( "replicate" %in% names(dat) ) { 
    cols <- c("replicate", cols)
    pretty_names <- c('Mat. #', pretty_names)
  }
  
  # Extract data and rename cols
  dat <- dat[, cols]
  names(dat) <- pretty_names
  
  return(dat)
}

#'@export
summary.patchdistr_spews <- function(object, ...) { 
  dat <- prepare_summary_table(object)
  
  cat('Patch-based Early-Warnings results\n') 
  cat('\n')
  print.data.frame(dat, row.names = FALSE, digits = DIGITS)
  cat('\n')
  cat('Use as.data.frame() to retrieve values in a convenient form\n')
  invisible(dat)
}



# Print methods
# --------------------------------------------------
# Note this method works for both single and lists objects so we only 
#   define it for patchdistr_spews (and not their *_list *_single 
#   equivalents)

#'@export
print.patchdistr_spews <- function(x, ...) { 
  cat('Patch-based Early-Warnings results\n') 
  cat('\n')
  
  print.data.frame( as.data.frame(x), row.names = FALSE)
  
  cat('\n')
  cat('Use as.data.frame() to retrieve values in a convenient form\n')
}

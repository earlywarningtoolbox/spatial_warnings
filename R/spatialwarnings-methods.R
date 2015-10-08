
# 
summary.spatialwarnings <- function(x) { 
  
  # If we applied the stuff on a list of binary matrices (e.g. data(L)), then we 
  #   apply the summary on everything
  
  if ( attr(x, 'is_replicated') ) { 
    output <- summary.spatialwarnings_with_replicates(x)
    
  }
  
  # 
  
} 

summary.spatialwarnings_with_replicates <- function(x, add_non_numerical = TRUE) { 
  
  # Extract info
  results <- x[['result']]
  oneresult <- results[[1]][['indicators']]
  
  # Get a list of indicator names
  indicators <- x[['indicator_names']]
  
  # For all numerical indicators, we fill in a data.frame with their values
  num_indics <- data.frame()
  for (indic in indicators) { 
    if ( is.numerical_indic(oneresult[[indic]]) ) { 
      # If it is a numerical indicator, then we get its value for all replicates
      allvalues <- lapply(results, function(x) { 
                     as.data.frame(x[['indicators']][[indic]])
                   })
      allvalues <- do.call(rbind, allvalues)
      num_indics <- plyr::rbind.fill(num_indics,
                              data.frame(indicator = indic, 
                                          replicate = seq.int(nrow(allvalues)),
                                          allvalues))
    }
  }
  
  if ( add_non_numerical ) { 
    # For all non-numerical indicators, we call their summary function
    nn_indics <- list()
    for (indic in indicators) { 
      if ( ! is.numerical_indic(oneresult[[indic]]) ) { 
        newnames <- c(names(nn_indics), indic)
        nn_indics <- c(nn_indics, 
                       lapply(results, function(x) { 
                         summary(x[['indicators']][[indic]])
                       }))
        names(nn_indics) <- newnames
      }
    }
    
  }
  
  output <- list(num_indicators = num_indics, 
                 nonnum_indicators = nn_indics)
  class(output) <- c('summary_spatialwarnings', 'list')
  return(output)
}



# Helper: test whether an indicator is numerical given its results
is.numerical_indic <- function(x) { 
  # Test whether it has a value component or if it just a single, numeric value
  'value' %in% names(x) || is.numeric(x) || is.integer(x) 
}

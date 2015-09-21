
#' @title Print a spatial indicator object
#' 
#' @description Print method for `spindic` objects (spatial 
#'   indicators).
#'   
#' @param obj An `spindic` object
#'
#' @param verbose If `TRUE` then print all indicator objects using 
#'   `print.default`
# 
#'@export
print.spindic <- function(obj, verbose = FALSE) {
  cat('Spatial indicator object (',length(obj), ' indicators):\n', sep ="")
  cat('\n')
  cat("Call:", deparse(obj[['call']]), "\n")
  cat('\n')
  cat('Indicator values:\n')
  
  # Print indicators values
  for (name in names(obj[['indicators']])) { 
    indic <- obj[['indicators']][[name]]
    cat(name, ': \t', sep ='')
    if ('value' %in% names(indic)) { 
      cat(round(indic[['value']], digits = options()[['digits']]))
    } else if (is.numeric(indic)) { 
      cat(round(indic, digits = options()[['digits']]))
    } else {
      cat('<',class(indic),' object>', sep='')
    }
    cat('\n')
  }
  
  # Print a lot of stuff if the user asked for it
  if (verbose) print.default(obj)
    
  # Rerturn the object invisibly 
  invisible(obj)
}



# Tshe next two functions take care of computing and displaying a summary.
# summary.spindic computs a summary object, that is displayed by 
# print.summary.spindic. This way if the suer wants to extract info from the 
# summary object he can assign it to a persistent variable.
# 
# This is the structure adopted by the functions glm/lm() et al.
# 
# The structure of a summary.spindic object can be pretty loose as it should
# be only used by these two functions.
summary.spindic <- function(obj) { 
  
  warnings('Not implemented yet :)')
  
}

# Prints a summary.spindic object
print.summary.spindic <- function(obj) { 
  
  
} 
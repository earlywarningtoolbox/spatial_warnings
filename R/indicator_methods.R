
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
  for (name in names(a)) { 
    cat(name, ': \t', sep ='')
    if ('value' %in% names(obj[[name]])) { 
      cat(round(obj[[name]][['value']], digits = options()[['digits']]), "\n")
    } else if (is.numeric(obj[[name]])) { 
      cat(round(obj[[name]], digits = options()[['digits']]), "\n")
    } else {
      cat('<',class(obj[[name]]),'>\n', sep='')
    }
  }
  
  # Print a lot of stuff if the user asked for it
  if (verbose) print.default(obj)
    
  # Rerturn the object invisibly 
  invisible(obj)
}

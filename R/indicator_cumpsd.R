#' Get cumulative patch sizes.
#'
#' @param x A vector of patch sizes or a list of vectors.
#'
#' @param patchvec a vector of patch sizes as produced by 
#'   \code{\link{patchsizes}}
#'
#' @return A dataframe of the unique patch sizes \code{size}, the number of 
#'   patches of equal or larger size than each value in \code{size}, and a 
#'   probability that any given patch is equal or larger size than each value 
#'   in \code{size}. 
#' 
#' @examples
#' 
#' data(B)
#' indicator_cumpsd(B)
#' 
#' 
#' @export
#' 

indicator_cumpsd <- function(x = NULL, patchvec = patchsizes(x) ) {
  check_mat(x) # sanity checks for the passed matrix 

  # Handle list case
  if ( ! is.null(x) && is.list(x)) { 
    out <- lapply(x, indicator_cumpsd) 
    class(out) <- c('indicator_cumpsd', 'list')
    return(out)
  } 
  
  out <- data.frame(size = unique(patchvec))
  if(length(out$size) > 1) {
    out$n <- sapply(seq.int(out$size), function(i) sum(patchvec >= out$size[i]) ) 
    out$p <- out$n/length(patchvec)
  } else {
    out$p <- 1 
    out$n <- 1 
  }
  
  class(out) <- c('indicator_cumpsd', 'data.frame')
  return(out)
}

#' @export
summary.indicator_cumpsd <- function(x) { 
  
  cat('Cumulative patch-size distribution')
  
  # Handle list case: merge all data
  if ( inherits(x, 'list') ) { 
    cat(" (",length(x), " replicates):\n", sep="")
    x <- do.call(rbind, x)
  } else { 
    cat(':\n')
  }
  cat(' min size:', min(x[ ,'size']), '\n',
      'max size:', max(x[ ,'size']), '\n',
      sum(x[ ,'n']), "patches in total\n")
}

#' @title Plot a patch-size cumulative distribution
#'
#' @param x An `indicator_cumpsd` object as produced by `indicator_cumpsd`
#'
#' @param x.log Whether to use log scale on x axis
#' 
#' @param y.log Whether to use log scale on y axis
#'
#' @param add.line Adds a straight regression line to the plot 
#'
#' @param facets Produce one plot facet per replicate or merge them altogether
#'               in one graph.
#' 
#' @examples
#' 
#' # An example with a list
#' data(L)
#' result <- indicator_cumpsd(L)
#' plot(result) 
#' plot(result, facets = TRUE)
#'
#' @export

plot.indicator_cumpsd <- function(x, x.log = TRUE, y.log = TRUE, 
                                  add.line = TRUE, facets = FALSE) { 
  
  # Convert from list if required
  if ( inherits(x, 'list') ) { 
    for (r in seq_along(x)) {
      x[[r]] <- data.frame(replicate = r, x[[r]]) 
    }
    x <- do.call(rbind, x)
  }
  
  baseplot <- ggplot2::ggplot(x, ggplot2::aes(size, p)) + 
                ggplot2::geom_point() + 
                ggplot2::xlab('Patch size') + 
                ggplot2::ylab('Frequency') 
    
  if ( x.log ) { 
    baseplot <- baseplot + ggplot2::scale_x_log10()
  }
  
  if ( y.log ) { 
    baseplot <- baseplot + ggplot2::scale_y_log10() 
  }
  
  if ( add.line )  { 
    baseplot <- baseplot + ggplot2::geom_smooth(method='lm', se = FALSE, 
                                                linetype = 'dashed')
  }
  
  if ( facets ) { 
    baseplot <- baseplot + ggplot2::facet_wrap( ~ replicate)
  }
  
  return(baseplot)
}

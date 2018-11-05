
# This function takes a matrix and a returns a single value.
#'@export
raw_kbdm <- function(mat, subsize = 3) {
  
  if ( ! require(acss) ) { 
    stop(paste0('Computation of kbdm requires the package acss. Install it ', 
                'using install.packages("acss")'))
  }
  
  if ( subsize > 3 || subsize < 1 ) { 
    stop("subsize must be between 1 and 3.")
  }
  
  # Split matrix
  subsize <- subsize
  xs <- seq(1, nrow(mat), by = subsize)
  ys <- seq(1, ncol(mat), by = subsize)
  allblockns <- expand.grid(seq.int(length(xs)-1),
                            seq.int(length(ys)-1))
  all_substr <- Map(function(xblockn, yblockn) {
                      dict <- as.vector(mat[xs[xblockn]:(xs[xblockn+1]-1),
                                            ys[yblockn]:(ys[yblockn+1]-1)]) 
                      dict <- as.integer() 
                      dict <- paste(collapse = "")
                    },
                    allblockns[ ,1], allblockns[ ,2]) 
  all_substr <- unlist(all_substr)
  
  # Summarize the substrings
  counts <- table(all_substr)
  counts <- data.frame(string = names(counts),
                       multip = as.vector(counts),
                       kctm = acss(names(counts), alphabet = 2)[ ,1])

  # Compute Kbdm
  return( with(counts, sum(log2(multip) + kctm)) )
}
  
#'
#' @title Indicator based on Kolmogorov Complexity 
#' 
#' @description Compute the Kolmogorov Complexity on a set of matrices, 
#'   using the Block Decomposition Method. 
#'
#' @details WriteMe
#' 
#'@export 
kbdm_spews <- function(mat, 
                       subsize = 3) { 
  
  # This is a formatted function to compute the kbdm
  kbdmfun <- function(mat, subsize) { 
    result <- list(value     = raw_kbdm(mat, subsize), 
                   orig_data = mat, 
                   fun.args  = as.list(match.call(expand.dots = FALSE))[['...']], 
                   indicf = raw_kbdm)
    
    class(result) <- c('kbdm_sews', 'simple_sews_single', 'custom_sews', 'list')
    attr(result, "indicname") <- "Kbdm Complexity"
    return(result)
  }
  
  if ( is.list(mat) ) { 
    result <- parallel::mclapply(mat, kbdmfun, subsize)
    names(result) <- names(mat)
    class(result) <- c('kbdm_sews', 'simple_sews_list', 'custom_sews', 'list')
    attr(result, "indicname") <- "Kbdm Complexity"
  } else { 
    result <- kbdmfun(mat, subsize)
  }
  
  return(result)

}


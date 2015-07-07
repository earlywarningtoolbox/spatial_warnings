
#' Labelling of patches.
#' 
#' @param x A binary matrix or a list of binary matrices.
#' 
#' @return A matrix containing ID numbers for each connected patch, assuming
#'   4-cell neighborhood and periodic boundaries.
#'   
#' @importFrom caspr mapping 
#' 
#' @details The function is written in R and depends on the \code{mapping()}
#'   function of package caspr.
#'   
#' @export
#' 
#' @example 
#'   data(B)
#'   par(mar=c(0,0,0,0))
#'   image(B, xaxt = "n", yaxt = "n", asp = 1, bty = "n", col = c("white", "black"))
#'   M <- label(B)
#'   image(M, xaxt = "n", yaxt = "n", asp = 1, bty = "n", 
#'      col = rep(rainbow(24), 100) )


label <- function(mat) {
  
  if("list" %in% class(mat)){ 
    lapply(mat, label) 
  } else {
    
    width <- dim(mat)[1]
    height <- dim(mat)[2]
    caspr::mapping(width, height)
    map <- matrix(rep(NA, times = prod(dim(mat))), ncol = width)
    old <- matrix(rep(99, times = prod(dim(mat))), ncol = width) 
    
    while(!identical(old, map)) {
      old <- map
      count = as.integer(1)
      for(i in which(mat)) {
        neighbors <- map[x_with_border][x_to_evaluate[i]+interact]
        if(all(is.na(neighbors)) ) { 
          map[i] <- count
        } else {
          map[i] <- min(neighbors, na.rm = TRUE)
        }
        count <- count +1
      }
      
    }
    
    return(map)
    
  }
} 



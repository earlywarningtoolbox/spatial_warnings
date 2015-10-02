#' @title Spatial warning indicators: Fractal Geometry
#'   
#' @description function to calculate fractal geometries. Also calculates patch
#'   sizes (same as patch area).
#'   
#' @param mat A binary matrix or a list of binary matrices of length >=1.
#' @param patchids A integer matrix obtained from the function \code{\link{label}}.
#' 
#' @return A list (of area and perimeter values for each patch in the input
#'   marix) if input is a single matrix (OR) a list of lists (of area and
#'   perimeter values for each patch) in every matrix of input list
#'   
#' @export
indicator_fracgeo <- function(mat = NULL, patchids = label(mat) ){
  
  check_mat(mat) # checks if binary and sensible
  
  if ( ! is.null(mat) && is.list(mat) ) { 
    output <- lapply(mat, indicator_fracgeo) 
    class(output) <- c('indicator_fracgeo', 'list')
    return(output)
  }
  
  periFT <- 0
  areaFT <- 0
  a <- patchids 
  a[is.na(a)] <- 0
  b <- a  # clone matrix for perimeter determination
  L <- dim(a)
  
  # remove all cells that are enclosed by equal IDs
  for (i in 2:(L[1]-1)){
    for (j in 2:(L[2]-1)){
      if (a[i,j]>0){
        if ( b[i-1,j] == a[i,j] && 
             b[i,j-1] == a[i,j] &&  
             b[i+1,j] == a[i,j] && 
             b[i,j+1] == a[i,j] ) {
               
          a[i,j] <- 0
        }
      }
    }
  }
  
  
  for (i in 2:(L[1]-1)){
    # repeat for left border while correcting for periodic boundaries
    if ( a[i,1] > 0 ){
      if ( b[i-1,1] == a[i,1] && 
           b[i,2] == a[i,1]   && 
           b[i+1,1] == a[i,1] && 
           b[i,L[1]] == a[i,1] ) {
             
        a[i,1] <- 0
      }
    }
    # repeat for right border while correcting for periodic boundaries
    if ( a[i,L[1]] > 0){
      if ( b[(i-1),L[1]] == a[i,L[1]] && 
           b[i,1] == a[i,L[1]] && 
           b[(i+1),L[1]] == a[i,L[1]] && 
           b[i,(L[1]-1)] == a[i,L[1]] ) {
             
        a[i,L[1]] <- 0
      }
    }
    # repeat for top border while correcting for periodic boundaries
    if ( a[1,i] > 0){
      if ( b[1,i-1] == a[1,i] && 
           b[2,i] == a[1,i] && 
           b[1,i+1] == a[1,i] && 
           b[L[1],i] == a[1,i] ) {
             
        a[1,i] <- 0
      }
    }
    # repeat for bottom border while correcting for periodic boundaries
    if ( a[L[1],i] > 0 ){
      if ( b[L[1],i-1] == a[L[1],i] && 
           b[L[1],i+1] == a[L[1],i] && 
           b[L[1]-1,i] == a[L[1],i] && 
           b[1,i] == a[L[1],i] ) {
             
        a[L[1],i] <- 0
      }
    }
  }
  
  # top-left corner
  if (a[1,1] > 0){
    if ( b[1,2] == a[1,1] && 
         b[1,L[1]] == a[1,1] && 
         b[2,1] == a[1,1] && 
         b[L[1],1] == a[1,1] ) {
           
      a[1,1] <- 0
    }
  }
         
  # top-right corner
  if (a[L[1],1] > 0) {
    if ( b[L[1]-1,1] == a[L[1],1] && 
         b[1,1] == a[L[1],1] && 
         b[L[1],2] == a[L[1],1] && 
         b[L[1],L[1]] == a[L[1],1] ) {
           
      a[L[1],1] <- 0
    }
  }
         
  # bottom-left corner
  if (a[1,L[1]] > 0) {
    if ( b[1,1] == a[1,L[1]] && 
         b[1,L[1]-1] == a[1,L[1]] && 
         b[2,L[1]] == a[1,L[1]] && 
         b[L[1],L[1]] == a[1,L[1]] ) {
           
      a[1,L[1]] <- 0
    }
  }
  
  # top-left corner
  if (a[1,1] > 0) {
    if ( b[1,2] == a[1,1] && 
         b[1,L[1]] == a[1,1] && 
         b[2,1] == a[1,1] && 
         b[L[1],1] == a[1,1] ) {
           
      a[1,1] <- 0
    }
  }
  
  # top-right corner
  if ( a[L[1],1] > 0 ) {
    if ( b[L[1]-1,1] == a[L[1],1] && 
         b[1,1] == a[L[1],1] && 
         b[L[1],2] == a[L[1],1] && 
         b[L[1],L[1]] == a[L[1],1] ) {
      a[L[1],1] <- 0
    }
  }
  # bottom-left corner
  if (a[1,L[1]]>0){
    if ( b[1,1] == a[1,L[1]] && 
         b[1,L[1]-1] == a[1,L[1]] && 
         b[2,L[1]] == a[1,L[1]] && 
         b[L[1],L[1]] == a[1,L[1]] ) {
      a[1,L[1]] <- 0
    }
  }
  # bottom-right corner
  if (a[L[1],L[1]]>0){
    if ( b[L[1]-1,L[1]] == a[L[1],L[1]] && 
         b[L[1],1] == a[L[1],L[1]] && 
         b[L[1],L[1]-1] == a[L[1],L[1]] && 
         b[1,L[1]] == a[L[1],L[1]] ) {
      a[L[1],L[1]] <- 0
    }
  }
  
  uniq <- unique(as.vector(a))
  uniq <- uniq[uniq > 0]
  for (i in uniq) {
    periFT  <-  c(periFT,sum(a == i))
    areaFT  <-  c(areaFT,sum(b == i))
  }
  periFT <- periFT[periFT > 0]
  areaFT <- areaFT[areaFT > 0]
  patchindex <- seq_along(uniq)
  
  # Format output
  output <- data.frame(patch_id = uniq, 
                       area = areaFT, 
                       perimeter = periFT, 
                       area_perimeter_ratio = areaFT/periFT)
  class(output) <- c('indicator_fracgeo', 'data.frame')
  return(output)

}

# Summary method
summary.indicator_fracgeo <- function(x) { 
  cat('Fractal geometry indicator')
  
  # Handle list case: merge all data
  if ( inherits(x, 'list') ) { 
    cat(" (",length(x), " replicates):\n", sep="")
    x <- do.call(rbind, x)
  } else { 
    cat(':\n')
  }
  
  cat(' (min-max)\n')
  
  cat(' Patch area: ', min(x[ ,"area"]), "-", max(x[, "area"]), "\n", sep = '')
  cat(' Patch perimeter: ', min(x[ ,"perimeter"]), "-",
      max(x[, "perimeter"]), "\n", sep = '')
  cat(' Patch area/perimeter ratio: ', min(x[ ,"area_perimeter_ratio"]), "-", 
      max(x[, "area_perimeter_ratio"]), "\n", sep = "")
  
}

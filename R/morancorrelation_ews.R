# This function computes Moran's I which is a measure of spatial autocorrelation  
# 

# TODO: Find the original paper of this Moran's index formulation.
# 
#' @title Moran's I
#' 
#' @description This function computes the Moran's I, i.e. a measure of the spatial autocorrelation.
#' 
#' @param input A binary matrix or a list of binary matrix. 
#' 
#' @return An object of class <myclass>, or a list of objects of class <myclass>
#'         if `mat` was a list of binary matrices.
#'
#' @references P.A.P. Moran (1950) Notes on Continuous Stochastic Phenomena. Biometrika 37(1-2):17-23  
#'
#'@export

indicator_moran <- function(input){
	check_mat(input) # checks if binary and sensible
	if ( is.list(input)) { 
		return( lapply(mat, morrancorrelation_ews) )
	}
}

morancorrelation_ews <- function(input){
	m <- mean(as.vector(input)) # Mean
	v <- var(as.vector(input))  # Variance
	n <- (nrow(input)-1)        # Degree of freedom ?
		
	moranI <- 0
	for (i in 2:n){
		for (j in 2:n) {
			moranI <- moranI + (input[i,j]-m)*(input[i,j-1]+input[i,j+1]+input[i-1,j]+input[i+1,j]-4*m)	
		}
	}
	moranI <- moranI/(4*v*(n-2)*(n-2)) 
	return(moranI)
}



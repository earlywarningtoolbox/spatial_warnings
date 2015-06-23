morancorrelation_ews <- function(input)
	{
		m <- mean(as.vector(input))
		v <- var(as.vector(input))
		n <- (nrow(input)-1)
		
		moranI <- 0
		for (i in 2:n){
			for (j in 2:n) {
						moranI <- moranI + (input[i,j]-m)*(input[i,j-1]+input[i,j+1]+input[i-1,j]+input[i+1,j]-4*m)	
			}
		}
		moranI <- moranI/(4*v*(n-2)*(n-2))
		return(moranI)
	}

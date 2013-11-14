#Known issues/TO-DO.
#1. subsize has to be a divisor of dim of fullmatrix. Otherwise, convert it to a nearest divisor.

reducedmatrix = function(fullmatrix, subsize=5)
{
	N = dim(fullmatrix)[1]
	n = N/subsize
	
	submatrix = matrix(nrow=subsize, ncol=subsize);
	reddata = matrix(nrow=N/subsize, ncol=N/subsize);
	
	for (i in 1:n)
	{
		for (j in 1:n)
		{
				submatrix = fullmatrix[((i-1)*subsize+1):(i*subsize),((j-1)*subsize+1):(j*subsize)];
				reddata[i,j] = mean(as.vector(submatrix));
		}
	}	
	
	return(reddata)
}
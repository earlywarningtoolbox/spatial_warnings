#Function to reduce original matrix to submatrix by averaging
#Writen by Vishwesha Guttal, 7th Nov 2013.
#Modified by Sabiha Majumder

reducedmatrix_ews = function(fullmatrix, subsize)
{
	N = dim(fullmatrix)[1]
	n = floor(N/subsize)
	
	submatrix = matrix(nrow=subsize, ncol=subsize);
	reddata = matrix(nrow=n, ncol=n);
	
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

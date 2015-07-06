#' @title This is R equivalent of fftshift function in matlab.
#' @description This function is a dependency of rspec_ews.R and Indicator_SpectralFunction.R. It is called by rspec_ews.R.
#' @details Writen by Vishwesha Guttal, 7th Nov 2013.

#' @param Binary square matrix
#' @return shifted matrix
myfftshift <- function(X)
{
  nr=dim(X)[1]
  nc=dim(X)[2]
  shiftX = X
  if (nr != nc)
    print("Not a square matrix X")
  else
  {
    n=nc
    shift = floor(n/2)
    for (i in 1:n)
      for (j in 1:n)
      {
        a = (i+shift)%%n
        b = (j+shift)%%n
        if (a==0) a = n
        if (b==0) b = n
        shiftX[a,b] = X[i,j]
      }
  }
  return(shiftX)
}
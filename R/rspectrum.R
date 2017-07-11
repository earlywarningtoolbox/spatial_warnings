# 
# 
# Wrapper for rspectrum_cpp to take into account the number of threads
#
#'@export
rspectrum <- function(mat, nthreads = getOption("spw.threads")) { 
  rspectrum_cpp(mat, nthreads)
}

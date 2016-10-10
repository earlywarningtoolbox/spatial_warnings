# This is a compilable file. Convert it into an html document using 
# knitr::spin("./draft/example.R", format = 'Rmd')

#' # A gentle introduction to the spatialwarnings package

#' ## Installation
#' 
#' The obvious first step is to install the package: currently this is done 
#' by cloning the github repo and installing. Installing R packages from 
#' github repositories is done using functions from the package devtools, so 
#' we first install it if needed. 

# This installs and loads devtools if it can't be found on your computer
if ( !require(devtools) ) { 
  install.packages(devtools)
  library(devtools)
}

# We now install the spatialwarnings package
# 
# You might need to set up IISC proxy beforehand if needed 
#   (the proxy parameters might have changed since Dec. 2015)
# Sys.setenv(https_proxy = "proxy.iisc.ernet.in:3128")
# Sys.setenv(http_proxy  = "proxy.iisc.ernet.in:3128")
install_github('fdschneider/spatial_warnings')
library(spatialwarnings) # mind the absence of underscore ! 


#' ## Example datasets
#' 
#' We will use two datasets in this example: 
#'   - `forestdat`: Output from the Forest-gap model: a `list` of two things : 
#'     - `"parameters"`: A `data.frame` containing the simulation parameters (ten rows)
#'     - `"matrices"`: A list of 100x100 matrices, each of which corresponding 
#'  to one snapshot of the corresponding simulation taken around equilibrium. 
#'  For example, the first matrix corresponds to the simulation with the 
#'  parameters given in the first row of the dataset. 
#'   
#'   - `arid`: A set of matrices, which is a `list` of black and white 100x100 images 
#'  (defined with continous values)
#' 
#' Both these datasets are included with the package, so we can just load them 
#' using the function `data()`
data(forestdat)
data(arid)



#' We can now start computing indicators. But first let's have an overview of 
#' the structure of the package. 
#' 
#' # Package general workflow
#' 
#' The package is (currently) centered around three families of spatial 
#' indicators: Generic EWS, Spectral EWS and Patch-based EWS. For each of these
#' families, a three-steps workflow is offered: 
#'   1. Compute the indicators: function `*_spews` (name depends on the 
#'   indicator considered)
#'   2. Assess their significance: function `indictest()`
#'   3. Plot the results: function `plot()`
#' 
#' At steps 1 and 2, standard methods such as `print()` or `summary()` are 
#' provided so that the user can have a look at the results textually. For some 
#' indicators, this workflow is extended further (e.g. multiple plot types).
#' 
#' The basic input data is a matrix with numeric values. Some functions or 
#' workflows have specific requirements (e.g. square matrix, binary values) and 
#' will warn if the input data is inadequate. Of course, computing these 
#' indicators on a single matrix often does not make sense, as we are 
#' most often interested in indicators *trends*. To help computing indicators 
#' on a bunch of matrices at once, all `*_spews` function can accept list of 
#' matrices. These list are checked for consistency before starting the 
#' computations (same size, same data type, etc.). 
#' 
#' Let's start with generic indicators. 
#' 




#' # Generic EWS
#' 
#' We will use the `forestdat` dataset to illustrate the computation of generic 
#' EWS. Let's see first what the data is like: 
  
#' Forestdat is a list of two components: 
#'   - the first one is a `data.frame`
forestdat[["parameters"]]
#'   - the second one is a list of matrices. As printing it takes too much space
#' for our small screens and brains, we use the handy function str() that 
#' produces a compact description of our object. 
str(forestdat[["matrices"]], 
    list.len = 3) # Show only the three first elements


#' The general function is called generic_spews: let's call this function on 
#' our list of matrices and store the result in a variable
forest.genic <- generic_spews(forestdat[["matrices"]])

#' We can now call the `summary()` method on this object to display the results. 
#' Note that the summary method does not reflect the internal structure of the 
#' `forest.genic` object (try using `str()` on forest.genic to see what data 
#' is kept in memory). 
summary(forest.genic)

#' We can plot these results right away...
plot(forest.genic) 

#' Or assess significance before plotting. This is done by shuffling the 
#' original matrix and recomputing the indicators many times. We store the 
#' results of the significance assessement into another variable. 
forest.gentest <- indictest(forest.genic)

#' And we call the summary function to see what is significant
summary(forest.gentest)

#' What does the trend look like ? Now a grey ribbon is added: it shows the 
#'  5% and 95% percentiles of the null distribution. 
plot(forest.gentest)

#' OK, that looks pretty good ! Of course, options are offered to tweak things 
#' a bit. A unique help page gathers all the information needed for the 
#' Generic EWS workflow: see `?generic_spews` 
#' 
#' For example, we can decide to use detrending (mean of matrix is substracted 
#' after coarse-graining), or that the computation of the Moran's I index 
#' should be carried out on coarse-grained data. We can also use the absolute 
#' value of skewness instead of its raw value. And last but not least, we can 
#' change the coarse-graining length. 
#' 
forest.genic <- generic_spews(forestdat[['matrices']], 
                              subsize = 3,
                              detrend = FALSE,
                              abs_skewness = TRUE,
                              moranI_coarse_grain = TRUE)

#' We can also specify the number of replicates to use in the null distribution
forest.gentest <- indictest(forest.genic, null_replicates = 1999)
plot(forest.gentest) # Compare with figure above 

#' Of course, at anytime during this process, we can export the values to 
#' a `data.frame` for further plotting
head( as.data.frame(forest.gentest) )




#' # Spectral EWS
#'
#' We will be using the same forestdat dataset to illustrate the spectral EWS
#' workflow. It works just the same: 
forest.specic <- spectral_spews(forestdat[["matrices"]])

#' Mind the warnings here telling us that we are using default values for SDR. 
#' Let's see a quick summary of the results. 
summary(forest.specic)

forest.spectest <- indictest(forest.specic)
plot(forest.spectest)
# Note that the indictest method here is quite slow as computing the 
# rspectrum is expensive (replace with math. approx ?)

#' Now, as you can see the `plot()` method displays the SDR trend, that 
#' summarizes the rspectrum values into one summary variable. If one want to 
#' have the full spectrum information, it can be displayed using 
#' `plot_spectrum`. 
plot_spectrum(forest.spectest)




#' # Patch-size distribution-based spatial indicators
#' 
#' 
data(forestdat2)

arid.psdic <- patchdistr_spews(forestdat2[['matrices']],
                               .progress = 'none')

a <- patchsizes(forestdat2[["matrices"]])

#' For this example, we will use aerial images of bushes (from Spain). The 
#' dataset is called arid (see Introduction). The first step to compute 
#' psd-based spatial indicators is to convert the dataset to binary values. This 
#' is enforced in this workflow by checking that all input matrices are of 
#' logical type (TRUE/FALSE). 
#' 
#' The thresholding itself is a complicated matter and is left to the user: we 
#' do not provide any specific function. So let's write our own.
classify_one_matrix <- function(mat) { 
  # We consider vegetation to occur when:
  a <- mat > quantile(mat, .45)
  print(percolation(a))
  a
}

# Now we have a thresholded dataset
arid.bw <- lapply(arid, classify_one_matrix)
image(arid.bw[[1]])


arid.psdic <- patchdistr_spews(arid.bw, .progress = "time")

summary(arid.psdic)

plot_distr(arid.psdic)


forest.psdic <- patchdistr_spews(forestdat$matrices)

summary(forest.psdic)




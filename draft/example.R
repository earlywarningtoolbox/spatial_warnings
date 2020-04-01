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
install_github('spatial-ews/spatialwarnings')
library(spatialwarnings) # mind the absence of underscore ! 


#' ## Example datasets
#' 
#' `forestgap`: An output from the Forest-gap model. It has matrix sizes 
#'  of 400x400. 
#' 
#' All these datasets are included with the package, so we can just load them 
#' using the function `data()`
data(forestgap)



#' We can now start computing indicators. But first let's have an overview of 
#' the structure of the package. 
#' 
#' # Package general workflow
#' 
#' The package is (currently) centered around three families of spatial 
#' indicators: Generic EWS, Spectral EWS and Patch-based EWS. For each of these
#' families, a three-steps workflow is offered: 
#'   1. Compute the indicators: function `*_spews` (name depends on the 
#'   indicator family considered)
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
#' We will use the `forestgap` dataset to illustrate the computation of generic 
#' EWS. Let's see first what the data is like: 

#' A data.frame of the parameters used for the simulations:
forestgap.pars

#' forestgap: a list of matrices. As printing takes too much space
#' for our small screens, we use the handy function str() that 
#' produces a compact description of our object. 
str(forestgap, 
    list.len = 3) # Show only the three first elements


#' The general function is called generic_spews: let's call this function on 
#' our list of matrices and store the result in a variable
forest.genic <- generic_spews(forestgap)

#' We can now call the `summary()` method on this object to display the results. 
#' Note that the summary method does not reflect the internal s*tructure of the 
#' `forest.genic` object (try using `str()` on forest.genic to see what data 
#' is kept in memory). 
summary(forest.genic)

#' We can plot these results right away...
plot(forest.genic) 

#' Or assess significance before plotting. This is done by shuffling the 
#' original matrix and recomputing the indicators many times. We store the 
#' results of the significance assessement into another variable. 
forest.gentest <- indictest(forest.genic, .progress = "time")

#' And we call the summary function to see what is significant
summary(forest.gentest)

#' What does the trend look like ? Now a grey ribbon is added: it shows the 
#'  5% and 95% percentiles of the null distribution. 
plot(forest.gentest)

#' OK, that looks pretty good ! Of course, options are offered to tweak things 
#' a bit. A unique help page gathers all the information needed for the 
#' Generic EWS workflow: see `?generic_spews` 
#' 
#' For example, we can that the computation of the Moran's I index 
#' should be carried out on coarse-grained data. We can also use the absolute 
#' value of skewness instead of its raw value. And last but not least, we can 
#' change the coarse-graining length. 
#' 
forest.genic <- generic_spews(forestgap, 
                              subsize = 3,
                              abs_skewness = TRUE,
                              moranI_coarse_grain = TRUE)

#' We can also specify the number of replicates to use in the null distribution
forest.gentest <- indictest(forest.genic, 
                            .progress = "time", 
                            nulln = 199)
plot(forest.gentest) # Compare with figure above 

#' Of course, at anytime during this process, we can export the values to 
#' a `data.frame` so we can reuse them in other contexts. 
head( as.data.frame(forest.gentest) )

#' Individual indicator functions are also available. They can also work 
#' on lists of matrices. Note that these functions are here so indicators 
#' can be computed more easily as part of another workflow: no 
#' plot/summary/print/etc. methods are provided. 
forest.varic <- indicator_variance(forestgap)
# See also indicator_skewness, indicator_moran, etc. 

#' Parallel processing is available via setting the global option spw.threads
options(spw.threads = 24)



#' # Spectral EWS
#'
#' We will be using the same forestgap dataset to illustrate the spectral EWS
#' workflow. It works just the same: 
forest.specic <- spectral_spews(forestgap)

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

#' Some options are available to alter how the indicator values are computed
forest.specic <- spectral_spews(forestgap, 
                                sdr_low_range =  c(0, .2), 
                                sdr_high_range = c(0, 1))
plot(forest.specic)
#' See also `?spectral_spews`





#' # PSD-based spatial indicators
#' 
#' 
#' We will use forestgap for the first part of this example. Let's compute 
#' these indicators. 
#'
data(forestgap)
for (i in rev(seq_along(forestgap))) { 
  print(i)
  patchdistr_spews(forestgap[[i]])
}

patchdistr_spews(forestgap[[35]])

forest.psdic <- patchdistr_spews(forestgap, fit_lnorm = FALSE)

#' See a summary of what has been fitted
summary(forest.psdic)

#' And plot the indicator values. This is quite a complex figure that 
#' summarises percolation status + psd type + powerlaw range. It is still 
#' a bit rough so sorry for this. 
#'  - Colored bars (or area if the x-axis has continous values) represents the 
#'  best distribution type (based on AIC, see ?patchdistr_spews). 
#'  - Lines indicate percolation of full sites (continous line) and empty 
#'  sites (dashed line). 
#'  - Gray bars in the other facet indicate Power-law range (ranging from near 0 
#'  to 100%)
#'  
#+fig.width=16,fig.height=5
plot(forest.psdic) # try with the along parameter

#' We can also plot the distributions to have a better view of what is going 
#' on. 
#+fig.width=16,fig.height=16
plot_distr(forest.psdic)


#' Of course, individual indicators are also available indvidually, through 
#' their own functions 

# Not run:
# indicator_plrange(forestgap)

#' That's it ! For most of these functions, documentation is available 
#' through `?` ! It is sometimes missing though (help is gladly accepted ;) ). 
#' This document should come with an editable R file example.R, do not hesitate
#' to go back to that file and edit things around. 
#' 



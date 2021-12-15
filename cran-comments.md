Dear CRAN maintainers, 

I hope you are having a good end of year. 

This is a minor update to the spatialwarnings package to fix a few bugs and
inaccurracies in computations.

Thanks in advance, 

Alexandre Génin



## Test environments

This package was tested using the following environments: 

 * R-lib Github actions (macOS with R-latest, windows with R-latest, 
    ubuntu-latest with R-devel, ubuntu-latest with R-release, 
    ubuntu-latest with R-oldrel): 
     https://travis-ci.org/spatial-ews/spatialwarnings
 
 * Local linux computer (Arch Linux as of 2021-12-14, R 4.1.2)
 
## R CMD check results

One remaining NOTE occurred on some platforms: 

The package size is sometimes reported as exceeding 1Mb (on ubuntu platforms 
above), probably due to the use of Rcpp: 

* checking installed package size ... NOTE
  installed size is  7.9Mb
  sub-directories of 1Mb or more:
    libs   6.6Mb

## Changes in this release

Improvements: 
  
  * Matrix attributes are now preserved when generating null matrices (except 
      when shuffling the original matrix, null_method = 'perm')
  
Bug fixes: 
  
  * Computation of Moran's I is now accurate for small matrices
  
  
  
## Package Description

spatialwarnings is a package that assists ecologists in carrying out 
computations of early-warning signals (EWS) of ecosystem degradation.

These EWS are based on the fact that some ecosystems are expected to show 
specific spatial patterns before undergoing non-linear transitions (a wide 
shift in their state despite a small change in external forcings). For example, 
such ecosystems are expected to show an increase in spatial autocorrelation, 
variance and skewness, or, for patchy ecosystems, specific changes in the patch 
size distribution.

This packages assists users with computing these metrics efficiently on matrix 
objects in R, test their significance based on randomizing spatial structure, 
and plot their trends based on ggplot2. A convenient, three-step workflow is 
provided based on summary/plot/etc. generic functions.

Homepage and usage example:

  https://github.com/spatial-ews/spatialwarnings

Reference:
  
  * Génin, A. , Majumder, S. , Sankaran, S. , Danet, A. , Guttal, V. , 
    Schneider, F. D. and Kéfi, S. (2018),
    Monitoring ecosystem degradation using spatial data and the R package 
    'spatialwarnings'. Methods Ecol Evol. 
    doi:10.1111/2041-210X.13058

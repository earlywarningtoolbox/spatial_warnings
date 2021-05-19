Dear CRAN maintainers, 

This is a major update to the package spatialwarnings that fixes bugs, improves 
the code base and provides new functionality. spatialwarnings is a package that 
assists ecologists in carrying out computations of early-warning signals (EWS) of 
ecosystem degradation. More information on the package and testing results are 
available below. 

Thanks in advance, 

Alexandre Génin



## Test environments

This package was tested using the following environments: 

 * Travis-ci (Ubuntu 16.04, R release (4.0.2) and devel (2021-05-17 r80314) ):
     https://travis-ci.org/spatial-ews/spatialwarnings
 
 * local linux computer (Arch Linux as of 2021-05-18, R 4.0.5)
 
 * win builder service (R release as of 2021-05-19)
 
 * Solaris x86 (Solaris 10 in local virtual machine, with manually-installed 
     R 4.0.0 and GNU gsl 2.6). 



## R CMD check results

No relevant ERRORs nor WARNINGs arose during testing on the above platforms. We
removed the unused dependency 'tidyr', which was the cause of an occasional NOTE 
on CRAN. 

One remaining NOTE occurred: 

The package size is sometimes reported as exceeding 1Mb (Solaris & Linux), 
probably due to the use of Rcpp: 

 * checking installed package size ... NOTE
    installed size is 5.1Mb
    sub-directories of 1Mb or more:
    libs 4.5Mb

## Changes in this release

Improvements: 

  * Documentation extended and improved
  
  * Fitting of distributions now uses 'plexpo' and 'trunc' to refer to the 
      exponent of a power-law ("slope") and its exponential truncation (from 
      "expo" and "rate", which were ambiguous)
  
  * EXPERIMENTAL: Variogram-based indicators have been added. Note that this 
      deserves more testing and application to real-world data to make sure that
      the computation of variograms are accurate enough. 
  
  * EXPERIMENTAL: Significance of flow-length can now be assessed using the 
      analytical approximation described in Rodriguez et al. (2017)
  
Bug fixes: 
  
  * Fixed a bug where available methods were not displayed in `summary()`
  
  * Fixed a bug where the r-spectrum was wrong when the matrix had an odd number 
      of rows or columns
  
Misc changes: 
  
  * NAs in provided matrices now produce warnings instead of errors
  
  * Dependency to tidyr has been removed (this fixes occasional NOTEs on CRAN
     automated checking)
  
Removals: 

  * All deprecated functions in spatialwarnings v2.0.0 are now defunct
  
  
## Package Description

spatialwarnings is a package that assists ecologists in carrying out 
computations of early-warning signals (EWS) of ecosystem degradation.

These EWS are based on the fact that some ecosystems are expected to show 
specific spatial patterns before undergoing non-linear transitions (e.g. a wide 
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

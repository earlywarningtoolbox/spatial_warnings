
This is an update to the package spatialwarnings that fixes many bugs, 
improves the code base and provides new functionality. More information is 
available below. 

Thanks, 

Alexandre Génin

## Test environments

This package was tested using the following environments: 

 - rhub service using the following platforms: 
  Debian Linux, R-devel, GCC ASAN/UBSAN
  Fedora Linux, R-devel, clang, gfortran
  Ubuntu Linux 16.04 LTS, R-release, GCC
  Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  
 (note that the package could not be tested on Solaris using the rhub 
  service because of a failure to compile Rcpp there)
  
 - Travis-ci (Ubuntu 14.04.5, R 3.5.0 and devel (2018-06-15 r74903) ):
   https://travis-ci.org/spatial-ews/spatialwarnings
 
 * local linux computer (Arch Linux as of 2020-05-13, R 4.0.0)
  
 - Solaris x86 (Solaris 11 in local virtualbox, R 3.3.0 from OpenCSW)



## R CMD check results

No ERRORs nor WARNINGs arose during testing on the above platforms. One NOTE 
occured regarding a spell check false positive: 

Possibly mis-spelled words in DESCRIPTION:
  al (9:322)
  et (9:319)
  EWS (9:82, 9:133)
  Genin (9:313)



## Changes in this release


Improvements: 
  * New methods are available to produce null matrices, on top of shuffling the
      original matrix (e.g. based on smoothing the original matrix). 
  * Significance of Power-law range can now be tested using `indictest()`. 
      Using `plot_distr()` on the resulting objects will display the 0.05/0.95 
      quantiles of the null patch size distributions. 
  * The package gains a generic function `display_matrix`, to eyeball the  
      matrices being used in `spatialwarnings` objects using ggplot2
  * Improved the fitting of distributions, which should be more robust now. 
  * Speed improvements in label()
  
Bug fixes: 
  * Fixed a bug where the normalization constant for truncated power-laws was 
      miscalculated
  
Removals: 
  * All the deprecated `*_spews` functions are now defunct (removed). 
  * Most `indicator_*` functions are now deprecated. 
  
Misc changes: 
  * Lots of duplicated code has been removed
  * Minor changes in print/summary methods
  * The FAQ has been expanded and improved. See the following page: 
      https://alex.lecairn.org/spatialwarnings-faq.html
  * Dependency to VGAM is now removed 
  
  
  
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

Dear CRAN maintainers, 

This is an update to the package spatialwarnings that fixes bugs, improves the 
code base and provides new functionality. spatialwarnings is a package that 
assists ecologists in carrying out computations of early-warning signals (EWS) 
of ecosystem degradation. More information on the package and testing results
are available below. 

Please note that the domain part of my email has changed following a 
reorganization of our university. I am not able to send emails from 
"@univ-montp2.fr" anymore, but I can receive mail at this old address and 
confirm the change if needed. 

Thanks in advance, 

Alexandre Génin

## Test environments

This package was tested using the following environments: 

 * rhub service using the following platforms: 
     Fedora Linux, R-devel, clang, gfortran
     Ubuntu Linux 16.04 LTS, R-release, GCC
     Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  
 * Travis-ci (Ubuntu 16.04, R 4.0.0 and devel (2020-05-13 r78449) ):
     https://travis-ci.org/spatial-ews/spatialwarnings
 
 * local linux computer, with and without valgrind (Arch Linux as of 
     2020-05-13, R 4.0.0)
 
 * win builder service (R 3.6.3, R 4.0.0, R devel 2020-05-11 r78411)
 
 * Solaris x86 (Solaris 10 in local virtual machine, with manually-installed 
     R 4.0.0 and GNU gsl 2.6). 



## R CMD check results

No ERRORs nor WARNINGs arose during testing on the above platforms. Several 
NOTEs occurred: 

On platforms Windows Server 2008 R2 SP1, R devel (rhub) and 
Ubuntu Linux 16.04 LTS, R-release (rhub), the following NOTE was produced, 
which I believe is a false positive: 
  
 * Found the following files/directories:
   'examples_i386' 'examples_x64' 'spatialwarnings-Ex_i386.Rout'
   'spatialwarnings-Ex_x64.Rout' 'tests_i386' 'tests_x64'

My email address has changed, reflecting the change in domain name of my 
university, hence the following NOTE: 

 * checking CRAN incoming feasibility ... NOTE
     Maintainer: 'Alexandre Genin <alexandre.genin@umontpellier.fr>'
     New maintainer:
       Alexandre Genin <alexandre.genin@umontpellier.fr>
     Old maintainer(s):
       Alexandre Genin <alexandre.genin@univ-montp2.fr>

The package size is reported as exceeding 1Mb, probably due to using Rcpp: 

 * checking installed package size ... NOTE
    installed size is 5.5Mb
    sub-directories of 1Mb or more:
    libs 4.5Mb



## Changes in this release

Improvements: 
  * New methods are available to produce null matrices, on top of shuffling the
      original matrix (e.g. based on smoothing the original matrix). 
  * Significance of Power-law range can now be tested using `indictest()`. 
      Using `plot_distr()` on the resulting objects will display the 0.05/0.95 
      quantiles of the null patch size distributions. 
  * The package gains a generic function `display_matrix`, to eyeball the  
      matrices being used in `spatialwarnings` objects 
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

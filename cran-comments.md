
This is an update to the package spatialwarnings that fixes many bugs, 
improve the code base and provides new functionality. 

## Changes in this release

New indicators: 
  * Planar flowlength (Mayor et al. 2013, Rodriguez et al. 2017)
  * Kolmogorov complexity based on Block Decomposition Method 
      (Dakos and Soler-Toscano 2016)
  
Improvements: 
  * Enable parallel computation of patch size distributions
  * Added a dataset of aerial view of vegetation in Arizona ('arizona')
  * Added functions to compute the coarse-grained variance/skewness on a 
      single matrix 
  
Bug fixes and code improvements: 
  * Added missing methods exports for custom indicators
  * Fixed the patch labelling for non-square images
  * General code cleanup and improvement
  
Documentation and description changes: 
  * Updated references to reflect the publication of new paper presenting 
      the package <doi:10.1111/2041-210X.13058>
  
## Test environments

 - Travis-ci (Ubuntu 14.04.5, R 3.5.0 and devel (2018-06-15 r74903) ):
   https://travis-ci.org/spatial-ews/spatialwarnings/builds/393408917
   
 - local linux computer (Arch Linux as of 2018-12-15, R 3.5.1)
   
 - Windows building service (R-devel at win-builder.r-project.org)
   https://win-builder.r-project.org/F6IHE3vX15aC
   
 - Solaris: r-hub building service (platform solaris-x86-patched)
   http://builder.r-hub.io/status/spatialwarnings_1.2.tar.gz-2fa883a51fff4e15bf80c194d4c3b0da

## R CMD check results

A note concerning a spell check false positive (my name): 

Possibly mis-spelled words in DESCRIPTION:
  Genin (9:313)


## Compiling Warnings 

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
    Monitoring ecosystemdegradation using spatial data and the R package 
    'spatialwarnings'. Methods Ecol Evol. 
    doi:10.1111/2041-210X.13058

  * Kéfi, S., Guttal, V., Brock, W.A., Carpenter, S.R., Ellison, A.M., Livina, 
    V.N., et al. (2014). Early Warning Signals of Ecological Transitions: 
    Methods for Spatial Patterns. PLoS ONE, 9, e92097.
    http://dx.plos.org/10.1371/journal.pone.0092097


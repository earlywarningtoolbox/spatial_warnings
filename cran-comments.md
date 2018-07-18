
This is a minor update to the package spatialwarnings that should fix 
compilation on Solaris, as requested by CRAN maintainers (problem regarding 
overloading ambiguity). It also fixes a small bug regarding functionality. 

Note that the previous version of the package has been archived on CRAN due to 
delays in providing the update.

## Changes in this release

Bug fixes: 
  * Compilation errors should now be fixed on Solaris
  * Fixed coarse-graining bug when input values are not integers
    
## Test environments

 - Travis-ci (Ubuntu 14.04.5, R 3.5.0 and devel (2018-06-15 r74903) ):
   https://travis-ci.org/spatial-ews/spatialwarnings/builds/393408917
   
 - local linux computer (Arch Linux as of 2018-06-17, R 3.5.0)
   
 - Windows building service (R-devel at win-builder.r-project.org)
   https://win-builder.r-project.org/F6IHE3vX15aC
   
 - Solaris: r-hub building service (platform solaris-x86-patched)
   http://builder.r-hub.io/status/spatialwarnings_1.2.tar.gz-2fa883a51fff4e15bf80c194d4c3b0da

## R CMD check results

There were no ERRORs or WARNINGs on any of the above platforms.

One NOTE was produced on win-builder, related to the fact that this package has 
been archived on 2018-06-17, along with a few false positives regarding 
mis-spelled words: 

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alexandre Genin <alexandre.genin@univ-montp2.fr>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  EWS (9:82, 9:133)
  Kefi (9:313)
  al (9:321)
  et (9:318)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2018-06-17 as 
    installation errors were not corrected despite reminder
  Overloading on Solaris.

## Compiling Warnings 

Some compilation warnings are produced on Windows in RcppArmadillo-related 
code, e.g.: 

d:/Compiler/gcc-4.9.3/mingw_32/bin/g++  -I"D:/RCompile/recent/R/include" -DNDEBUG  -I"d:/RCompile/CRANpkg/lib/3.6/Rcpp/include" -I"d:/RCompile/CRANpkg/lib/3.6/RcppArmadillo/include"   -I"d:/Compiler/gcc-4.9.3/local330/include"     -pedantic -O2 -Wall  -mtune=core2 -c RcppExports.cpp -o RcppExports.o
In file included from d:/RCompile/CRANpkg/lib/3.6/RcppArmadillo/include/armadillo:406:0,
                 from d:/RCompile/CRANpkg/lib/3.6/RcppArmadillo/include/RcppArmadilloForward.h:46,
                 from d:/RCompile/CRANpkg/lib/3.6/RcppArmadillo/include/RcppArmadillo.h:31,
                 from RcppExports.cpp:4:
d:/RCompile/CRANpkg/lib/3.6/RcppArmadillo/include/armadillo_bits/arma_ostream_meat.hpp:74:7: warning: integer constant is too large for 'long' type [-Wlong-long]
       ( cond_rel< (sizeof(eT) > 4) && (is_same_type<uword,eT>::yes || is_same_type<sword,eT>::yes) >::geq(val, eT(+10000000000)) )
       ^

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

